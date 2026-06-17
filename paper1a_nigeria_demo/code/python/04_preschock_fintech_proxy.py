"""
Script 04: Pre-Shock Fintech Density Proxy from NLPS Round 6
Paper 1a: Nigeria Demonetization

Constructs the key right-hand-side variable: state-level digital financial
services penetration as of October 2022 (11 days before the CBN announcement).

Why this is better than CBN PSS data:
  - Already in our dataset — no additional download
  - Household-level survey → directly measures actual usage, not just infrastructure
  - Dated Oct 15, 2022 — cleanest possible pre-announcement baseline
  - Allows heterogeneity within states (urban/rural, income)

Service codes in NLPS Round 6 sect_5 (identified from penetration rates):
  Code | R6 penetration | Most likely service
  ---- | -------------- | -------------------
    1  |  0.7%          | Pension / formal insurance
    2  |  0.0%          | Securities / investment account
    3  |  5.9%          | Mobile money (MMO: OPay, PalmPay, MTN MoMo)
    4  |  3.4%          | Microfinance bank / cooperative
    5  | 14.7%          | Bank account (formal commercial bank)
    6  | 86.2%          | Mobile phone / USSD (*737#, *901# etc.)
    7  |  5.1%          | Mobile banking app / internet banking
    8  |  0.0%          | Credit card

NOTE: Service code labels are inferred from penetration rates. Download the
NLPS Phase 2 questionnaire from the World Bank microdata catalog to confirm.
Variable s5fq4 = 1 (YES) / 2 (NO) for each service.

Output: data/instruments/preschock_fintech_state.csv
  Columns: state_code, state_name, n_hh,
           pct_mobile_phone (code 6),
           pct_bank_account (code 5),
           pct_mobile_money (code 3),
           pct_mobile_banking (code 7),
           fintech_index (composite: codes 3+5+6+7)

Install: pip install pandas numpy
"""

import pandas as pd
import numpy as np
from pathlib import Path

ROOT  = Path(__file__).parents[2]
NLPS  = ROOT / "data/raw/lsms_isa/nlps_phone_survey"
OUT   = ROOT / "data/instruments/preschock_fintech_state.csv"

# Service code interpretations (update if NLPS codebook confirms different labels)
SERVICE_LABELS = {
    1: "pension_insurance",
    2: "securities",
    3: "mobile_money",      # OPay / PalmPay / MTN MoMo
    4: "microfinance_coop",
    5: "bank_account",      # formal commercial bank
    6: "mobile_phone_ussd", # basic mobile / USSD (*737# etc.)
    7: "mobile_banking_app",
    8: "credit_card",
}

# Services that constitute "fintech readiness" for the cash-crunch context
FINTECH_SERVICES = [3, 5, 6, 7]  # mobile money + bank + USSD + mobile app

def load_round6():
    """Load NLPS Round 6 sect_5 (financial services) and sect_a (geography)."""
    # Financial services module
    s5 = pd.read_csv(NLPS / "p2r6_sect_5.csv")
    # Geography / household identifiers
    sa = pd.read_csv(NLPS / "p2r6_sect_a_2_5_6_8_11b_12.csv",
                     usecols=["hhid", "zone", "state", "lga", "sector",
                               "wt_p2round6"])
    return s5, sa


def build_hh_fintech(s5):
    """
    Pivot sect_5 from long to wide: one row per household,
    one column per service code.
    s5fq4 = 1 (YES), 2 (NO)
    """
    # Convert s5fq4 to binary 0/1
    s5["uses"] = (s5["s5fq4"].astype(str) == "1").astype(int)

    # Pivot: rows = hhid, columns = service codes
    wide = (s5.pivot_table(index="hhid",
                            columns="service_cd",
                            values="uses",
                            aggfunc="max")
              .reset_index())
    wide.columns = (["hhid"] +
                    [SERVICE_LABELS.get(int(c), f"service_{c}")
                     for c in wide.columns[1:]])

    # Composite fintech index: sum of key digital services (0-4)
    present_cols = [SERVICE_LABELS[c] for c in FINTECH_SERVICES
                    if SERVICE_LABELS[c] in wide.columns]
    wide["fintech_index"] = wide[present_cols].sum(axis=1)
    wide["any_digital_payment"] = (wide["fintech_index"] > 0).astype(int)

    return wide


def aggregate_to_state(hh_df, sa):
    """Merge with geography and compute weighted state-level means."""
    merged = hh_df.merge(sa, on="hhid", how="left")

    service_cols = list(SERVICE_LABELS.values()) + ["fintech_index", "any_digital_payment"]
    available = [c for c in service_cols if c in merged.columns]

    # Weighted mean per state (using round-6 survey weights)
    def weighted_mean(x, w):
        w = w.reindex(x.index).fillna(1)
        return np.average(x.dropna(), weights=w.loc[x.dropna().index])

    rows = []
    for state_code, grp in merged.groupby("state"):
        row = {"state_code": state_code,
               "state_name": grp["state"].iloc[0],
               "n_hh": len(grp)}
        for col in available:
            if col in grp.columns:
                w = grp.get("wt_p2round6", pd.Series(1, index=grp.index))
                row[f"pct_{col}"] = round(weighted_mean(grp[col], w) * 100, 2)
        rows.append(row)

    return pd.DataFrame(rows).sort_values("state_code")


def main():
    print("Loading NLPS Round 6 (Oct 15, 2022 — 11 days pre-shock)...")
    s5, sa = load_round6()

    print(f"  Households in sect_5: {s5['hhid'].nunique()}")
    print(f"  States covered: {sa['state'].nunique()}")

    print("\nBuilding household-level fintech profile...")
    hh_df = build_hh_fintech(s5)
    print(hh_df[list(SERVICE_LABELS.values())[:5]].describe().round(3))

    print("\nAggregating to state level (survey-weighted)...")
    state_df = aggregate_to_state(hh_df, sa)

    state_df.to_csv(OUT, index=False)
    print(f"\nSaved: {OUT}")
    print(f"States: {len(state_df)}")
    print("\nPre-shock fintech penetration by state (top 10 by fintech index):")
    key_cols = ["state_name", "pct_mobile_money",
                "pct_bank_account", "pct_mobile_phone_ussd",
                "pct_fintech_index"]
    present = [c for c in key_cols if c in state_df.columns]
    print(state_df.nlargest(10, "pct_fintech_index")[present].to_string(index=False))

    print("\nBottom 10 (lowest fintech penetration — most exposed to cash crunch):")
    print(state_df.nsmallest(10, "pct_fintech_index")[present].to_string(index=False))


if __name__ == "__main__":
    main()
