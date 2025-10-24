# Author: Jiaxin He (jiaxin@eig.org)
# Date last edited: 10.16.2025

#!/usr/bin/env python3

import argparse
import sys
import pandas as pd
import numpy as np
from typing import List, Optional, Tuple

from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity

# --------------------------
# Utilities
# --------------------------

def normalize_cols(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    df.columns = [c.strip() for c in df.columns]
    return df

def clean_text(s: Optional[str]) -> str:
    """Lowercase, basic cleanup, collapse spaces. Keep digits/letters and +/# (C++, C#)."""
    if s is None or pd.isna(s):
        return ""
    s = str(s).lower()
    out = []
    prev_space = False
    for ch in s:
        if ch.isalnum() or ch in ["+", "#"]:
            out.append(ch); prev_space = False
        else:
            if not prev_space:
                out.append(" "); prev_space = True
    return " ".join("".join(out).strip().split())

def expand_synonyms(q: str) -> str:
    
    q_ = f" {q} "
    expansions = []

    if "attorney" in q_:
        expansions += ["lawyers"]
        
    if "vice president" in q_:
        expansions += ["financial managers"]
      
    if "project manager" in q_ or "project management" in q_ or "project mgmt" in q_:
        expansions += ["project management specialists"]
    
    if ("finance associate" in q_ or "financial advisor" in q_ or
        "financial analyst" in q_ or "investment analyst" in q_):
        expansions += ["financial and investment analysts", "investment analysts"]

    if "quant" in q_ or "quantitative" in q_:
        expansions += ["financial quantitative analysts", "quantitative analysts"]

    if "java" in q_ or "python" in q_:
        expansions += ["software developer", "software", "developer"]

    if "business intelligence" in q_:
        expansions += ["business operations specialists all other"]

    if "technical" in q_ or "tech lead" in q_ or "lead engineer" in q_:
        expansions += ["software developer", "computer programmer"]

    if ("full stack" in q_ or "frontend" in q_ or "front end" in q_ or
        "backend" in q_ or "back end" in q_):
        expansions += ["software developer", "computer programmer"]

    if ("machine learning" in q_ or " ml " in q_ or "statistic" in q_ or
        "data science" in q_ or "data scientist" in q_):
        expansions += ["data scientist", "statistician"]

    if ("software quality" in q_ or "software tester" in q_ or
        "quality assurance tester" in q_ or "quality assurance analyst" in q_):
        expansions += ["software quality assurance analysts and testers"]

    if "paralegal" in q_ or "legal assistant" in q_:
        expansions += ["paralegal and legal assistant"]

    if ("marketing research" in q_ or "market research" in q_ or
        "marketing analyst" in q_ or "market analyst" in q_ or
        "marketing specialist" in q_ or "market specialist" in q_ or
        "marketing intelligence" in q_ or "market intelligence" in q_):
        expansions += ["market research analyst and marketing specialist"]

    if "data" in q_ and "manager" in q_:
        expansions += ["computer and information systems managers"]

    if "software" in q_ and "manager" in q_:
        expansions += ["computer and information systems managers"]

    if expansions:
        q = q + " " + " ".join(expansions)
    return q

def build_vectorizer(corpus: List[str]) -> TfidfVectorizer:
    """Deterministic TF-IDF (unigram+bigram) for cosine similarity search."""
    vec = TfidfVectorizer(
        lowercase=True,
        ngram_range=(1, 2),
        min_df=1,
        max_df=1.0,
        norm="l2",
        use_idf=True,
        smooth_idf=True,
        sublinear_tf=False,
        dtype=np.float32
    )
    vec.fit(corpus)
    return vec

def score_candidates(job_vec, cand_matrix, cand_df: pd.DataFrame, top_k: int = 3):
    """
    Cosine similarity of one job_vec vs ALL candidates.
    Returns list of (score, SOC, soc_title_norm, dot_title_norm), deterministically sorted.
    """
    if cand_matrix.shape[0] == 0:
        return []
    scores = cosine_similarity(job_vec, cand_matrix).ravel()
    recs = []
    for i, sc in enumerate(scores):
        row = cand_df.iloc[i]
        soc = str(row["SOC"])
        soc_title = str(row["soc_title_norm"]) if not pd.isna(row["soc_title_norm"]) else ""
        dot_title = str(row["dot_title_norm"]) if not pd.isna(row["dot_title_norm"]) else ""
        recs.append((float(sc), soc, soc_title, dot_title))
    recs.sort(key=lambda t: (-t[0], t[1], t[2], t[3]))
    return recs[:top_k] if top_k else recs


# --------------------------
# Main
# --------------------------

def main():
    parser = argparse.ArgumentParser(description="Global job-title to SOC matching via deterministic TF-IDF similarity.")
    parser.add_argument("--applicants", default="Applicants without SOC codes.csv",
                        help="Applicants CSV (expects columns: applicant_id, DOT_category, job_norm)")
    parser.add_argument("--xwalk", default="DOT_SOC_xwalk.csv",
                        help="Crosswalk CSV (expects columns: DOT_category, dot_title_norm, SOC, soc_title_norm)")
    parser.add_argument("--out", default="Applicants with SOC codes (semantic xwalk).csv",
                        help="Output CSV")
    parser.add_argument("--topk", type=int, default=3, help="Keep top-k alternatives")
    args = parser.parse_args()

    # Load
    apps = pd.read_csv(args.applicants)
    xw = pd.read_csv(args.xwalk)

    apps = normalize_cols(apps)
    xw = normalize_cols(xw)

    # Column resolution (light aliasing)
    def pick(df, names):
        cols_lower = {c.lower(): c for c in df.columns}
        for n in names:
            if n.lower() in cols_lower:
                return cols_lower[n.lower()]
        for c in df.columns:
            if any(n.lower() in c.lower() for n in names):
                return c
        raise ValueError(f"Missing required column among: {names}. Found: {list(df.columns)}")

    app_id_col  = pick(apps, ["applicant_id", "id"])
    app_dot_col = pick(apps, ["DOT_category", "dot_category", "dot"])   # not used in matching; just passed through
    app_job_col = pick(apps, ["job_norm", "job", "job_title_norm", "job_title"])

    x_dot_col   = pick(xw, ["DOT_category", "dot_category", "dot"])
    x_dot_title = pick(xw, ["dot_title_norm", "dot_title"])
    x_soc_col   = pick(xw, ["SOC", "soc", "soc_code"])
    x_soc_title = pick(xw, ["soc_title_norm", "soc_title"])

    # Clean / standardize inputs
    apps = apps[[app_id_col, app_dot_col, app_job_col]].copy()
    apps.rename(columns={app_id_col: "applicant_id",
                         app_dot_col: "DOT_category",
                         app_job_col: "job_norm"}, inplace=True)

    xw = xw[[x_dot_col, x_dot_title, x_soc_col, x_soc_title]].copy()
    xw.rename(columns={x_dot_col: "DOT_category",
                       x_dot_title: "dot_title_norm",
                       x_soc_col: "SOC",
                       x_soc_title: "soc_title_norm"}, inplace=True)

    apps["job_norm"] = apps["job_norm"].apply(clean_text)
    xw["dot_title_norm"] = xw["dot_title_norm"].apply(clean_text)
    xw["soc_title_norm"] = xw["soc_title_norm"].apply(clean_text)

    # SOC -> canonical title (mode) mapping for output
    soc_to_title = {}
    for soc, grp in xw.groupby(xw["SOC"].astype(str)):
        titles = grp["soc_title_norm"].dropna().astype(str)
        soc_to_title[soc] = titles.mode().iat[0] if not titles.empty else ""

    # Crosswalk search text and vectorizer (global corpus)
    xw["search_text"] = (xw["dot_title_norm"].fillna("") + " " + xw["soc_title_norm"].fillna("")).str.strip()
    corpus = xw["search_text"].tolist()
    vectorizer = build_vectorizer(corpus)
    xw_matrix_full = vectorizer.transform(corpus)  # (N_xwalk x d)
    xw_view = xw[["dot_title_norm", "SOC", "soc_title_norm", "search_text"]].reset_index(drop=True)

    # Process applicants (global search only)
    out_records = []

    for _, row in apps.iterrows():
        app_id = row["applicant_id"]
        dot    = row["DOT_category"]   # passed through only
        job    = row["job_norm"]

        job_clean    = clean_text(job)
        job_expanded = expand_synonyms(job_clean)

        if job_expanded.strip():
            job_vec = vectorizer.transform([job_expanded])
            scored = score_candidates(job_vec, xw_matrix_full, xw_view, top_k=args.topk)
            if scored:
                best_score, best_soc, best_soc_title, best_dot_title = scored[0]
                method = "global_similarity"
            else:
                # Extremely rare: empty crosswalk or degenerate vectorizer
                best_soc = None; best_soc_title = None; best_score = np.nan
                scored = []; method = "no_match"
        else:
            best_soc = None; best_soc_title = None; best_score = np.nan
            scored = []; method = "no_job_title"

        rec = {
            "applicant_id": app_id,
            "DOT_category": dot,
            "job_norm": job,
            "predicted_SOC": best_soc,
            "predicted_SOC_title": soc_to_title.get(best_soc, best_soc_title) if best_soc else None,
            "similarity_score": float(best_score) if not pd.isna(best_score) else np.nan,
            "method": method
        }

        # Top-k alternatives
        for k in range(args.topk):
            if k < len(scored):
                score_k, soc_k, soc_title_k, dot_title_k = scored[k]
                rec[f"alt{k+1}_SOC"] = soc_k
                rec[f"alt{k+1}_SOC_title"] = soc_to_title.get(soc_k, soc_title_k)
                rec[f"alt{k+1}_score"] = float(score_k)
            else:
                rec[f"alt{k+1}_SOC"] = None
                rec[f"alt{k+1}_SOC_title"] = None
                rec[f"alt{k+1}_score"] = np.nan

        out_records.append(rec)

    out_df = pd.DataFrame(out_records)
    out_df.to_csv(args.out, index=False)
    print(f"Wrote {args.out} with {len(out_df):,} rows.")
    # Quick deterministic summary
    print(out_df["method"].value_counts(dropna=False).to_string(), file=sys.stderr)


if __name__ == "__main__":
    main()
