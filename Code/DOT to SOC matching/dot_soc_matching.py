# Author: Jiaxin He (jiaxin@eig.org)
# Date last edited: 10.16.2025

#!/usr/bin/env python3
import pandas as pd, numpy as np
from collections import Counter

from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder
from sklearn.impute import SimpleImputer
from sklearn.naive_bayes import MultinomialNB

TRAIN = "Applicants with SOC codes and titles.csv"
INFER = "Applicants without xwalk matches.csv"
OUT   = "Applicants with predicted SOC codes (learned from data).csv"

def normalize_cols(df):
    df = df.copy()
    df.columns = [c.strip() for c in df.columns]
    return df

def pick_col(df, candidates):
    cols_lower = {c.lower(): c for c in df.columns}
    for cand in candidates:
        if cand.lower() in cols_lower:
            return cols_lower[cand.lower()]
    # relaxed fallback: substring match
    for c in df.columns:
        if any(cand.lower() in c.lower() for cand in candidates):
            return c
    raise ValueError(f"None of {candidates} found in columns: {list(df.columns)}")

def clean_text_series(s):
    s = s.astype(str).str.strip()
    s = s.replace({"": np.nan, "nan": np.nan, "None": np.nan})
    return s

def standardize_dot(series):
    def to_dot(x):
        if pd.isna(x): return x
        digits = "".join(ch for ch in str(x) if ch.isdigit())
        if digits.isdigit():
            try: return f"{int(digits):03d}"
            except: return digits
        return str(x)
    return series.apply(to_dot)

def main():
    # ---- Load ----
    train_df = pd.read_csv(TRAIN)
    infer_df = pd.read_csv(INFER)

    train_df = normalize_cols(train_df)
    infer_df = normalize_cols(infer_df)

    # ---- Resolve columns (robust to small naming variations) ----
    DOT_col  = pick_col(train_df, ["DOT_category", "DOT", "dot_category", "dot_3digit", "DOT_3digit"])
    JOB_col  = pick_col(train_df, ["job_norm", "job_normalized", "job_title_norm", "job", "job_title"])
    EMP_col  = pick_col(train_df, ["registration_employer_name", "employer", "employer_name"])
    FLD_col  = pick_col(train_df, ["petition_beneficiary_field", "degree_field", "highest_degree_field", "education_field"])
    SOC_code_col  = pick_col(train_df, ["SOC_code", "soc_code", "soc2019", "soc"])
    SOC_title_col = pick_col(train_df, ["SOC_title", "soc_title", "soc2019_title", "soc_title_2019"])

    DOT_col_i = pick_col(infer_df, ["DOT_category", "DOT", "dot_category", "dot_3digit", "DOT_3digit"])
    JOB_col_i = pick_col(infer_df, ["job_norm", "job_normalized", "job_title_norm", "job", "job_title"])
    EMP_col_i = pick_col(infer_df, ["registration_employer_name", "employer", "employer_name"])
    FLD_col_i = pick_col(infer_df, ["petition_beneficiary_field", "degree_field", "highest_degree_field", "education_field"])

    # ---- Clean & normalize ----
    for c in [DOT_col, JOB_col, EMP_col, FLD_col]:
        train_df[c] = clean_text_series(train_df[c])
    for c in [DOT_col_i, JOB_col_i, EMP_col_i, FLD_col_i]:
        infer_df[c] = clean_text_series(infer_df[c])

    train_df[DOT_col] = standardize_dot(train_df[DOT_col])
    infer_df[DOT_col_i] = standardize_dot(infer_df[DOT_col_i])

    train_df[JOB_col] = train_df[JOB_col].astype(str).str.lower()
    infer_df[JOB_col_i] = infer_df[JOB_col_i].astype(str).str.lower()

    # ---- Deterministic One-Hot Encoding pipeline ----
    # Align inference columns to training names so the ColumnTransformer sees the same feature names
    infer_aligned = infer_df.rename(columns={
        DOT_col_i: DOT_col,
        JOB_col_i: JOB_col,
        EMP_col_i: EMP_col,
        FLD_col_i: FLD_col
    })

    cat_features = [DOT_col, JOB_col, EMP_col, FLD_col]
    y = train_df[SOC_code_col].astype(str).str.strip()

    # Fixed, sorted category lists from training data => deterministic column order
    categories_list = []
    for c in cat_features:
        vals = train_df[c].fillna("__missing__").astype(str).unique()
        categories_list.append(sorted(vals))

    ohe = OneHotEncoder(handle_unknown="ignore",
                    categories=categories_list,
                    sparse_output=True,
                    dtype=np.float32)

    preprocess = ColumnTransformer(
        transformers=[(
            "cat",
            Pipeline(steps=[
                ("imputer", SimpleImputer(strategy="constant", fill_value="__missing__")),
                ("ohe", ohe),
            ]),
            cat_features
        )]
    )

    # NB works well on one-hot (non-negative) features; swap to LogisticRegression for more expressive modeling if desired
    clf = MultinomialNB(alpha=1.0)
    pipe = Pipeline([("prep", preprocess), ("clf", clf)])

    # Fit on FULL training set for the production artifact (deterministic given same data/env)
    pipe.fit(train_df[cat_features], y)

    # Optional: persist the exact pipeline for future identical transforms
    # joblib.dump(pipe, "soc_model_onehot.joblib")

    # ---- Predict (top-1 + top-3) ----
    proba = pipe.predict_proba(infer_aligned[cat_features])
    pred_codes = pipe.predict(infer_aligned[cat_features])
    classes = pipe.named_steps["clf"].classes_

    top3_idx = np.argsort(-proba, axis=1)[:, :3]
    top3_codes = [[classes[i] for i in row] for row in top3_idx]
    top3_probs = [[float(proba[r, i]) for i in row] for r, row in enumerate(top3_idx)]

    # Map code -> (most common) SOC title from training
    code_to_title = {}
    grouped = train_df[[SOC_code_col, SOC_title_col]].dropna()
    for code, sub in grouped.groupby(SOC_code_col):
        code_to_title[str(code)] = sub[SOC_title_col].astype(str).str.strip().mode().iloc[0]

    pred_titles = [code_to_title.get(str(c), np.nan) for c in pred_codes]

    # ---- Output ----
    out_df = infer_df.copy()
    out_df["predicted_SOC_code"]  = pred_codes
    out_df["predicted_SOC_title"] = pred_titles
    for k in range(3):
        out_df[f"alt{k+1}_SOC_code"] = [row[k] if len(row) > k else np.nan for row in top3_codes]
        out_df[f"alt{k+1}_prob"]     = [row[k] if len(row) > k else np.nan for row in top3_probs]

    out_df.to_csv(OUT, index=False)
    print(f"Wrote {OUT} with {len(out_df):,} rows.")

if __name__ == "__main__":
    main()
