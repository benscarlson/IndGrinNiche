#!/bin/bash

#datName=huj_eobs
#cd ~/projects/whitestork/results/stpp_models/$datName

#TODO: it would be better if all sql was in one .sql script, then 
# this script called that script.
#TODO: should I really define my own PK, or just let sqlite manage that?

#TODO: don't overwrite if database already exists!!
touch data/database.db

sqlite3 data/database.db <<EOF
CREATE TABLE model_summary (
  model_summary_id INTEGER PRIMARY KEY,
  mod_name TEXT NOT NULL,
  niche_name TEXT NOT NULL,
  term TEXT NOT NULL,
  component TEXT NOT NULL,
  value REAL NOT NULL
);
EOF

sqlite3 data/database.db <<EOF
CREATE TABLE niche_stats (
  niche_stats_id INTEGER PRIMARY KEY,
  ses_id TEXT NOT NULL,
  rep INTEGER NOT NULL,
  niche_name TEXT NOT NULL,
  niche_vol REAL NOT NULL,
  rini REAL,
  spec REAL as (1-rini)
);
EOF

sqlite3 data/database.db <<EOF
CREATE TABLE niche_set_stats (
  niche_set_stats_id INTEGER PRIMARY KEY,
  ses_id TEXT NOT NULL,
  rep INTEGER NOT NULL,
  niche_set TEXT NOT NULL,
  niche_set_vol REAL NOT NULL,
  nestedness REAL,
  rini REAL,
  spec REAL as (1-rini),
  clust_w REAL
);
EOF

sqlite3 data/database.db <<EOF
CREATE TABLE pairwise (
  pairwise_id INTEGER PRIMARY KEY,
  ses_id TEXT NOT NULL,
  rep INTEGER NOT NULL,
  niche_name1 TEXT NOT NULL,
  niche_name2 TEXT NOT NULL,
  hv_intr_vol REAL NOT NULL,
  hv_union_vol REAL NOT NULL,
  hv_unique1_vol REAL NOT NULL,
  hv_unique2_vol REAL NOT NULL,
  nestedness REAL NOT NULL
);
EOF