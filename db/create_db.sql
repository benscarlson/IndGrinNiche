--- Starting this script in order to create new analysis databases
--- Primary script is still create_db.sh but I should merge taht script into this script

-- 2020-04-30 Create a table to hold repeatability scores
-- remember if not adding pk column, then automatic pk is added called 'rowid'
-- depending on trait, hvjob or mod might not be applicable
CREATE TABLE rpt (
  trait TEXT NOT NULL,
  R REAL NOT NULL
  conf_low Real NULL,
  conf_high Real NULL,
  mod TEXT NOT NULL,
  hvjob TEXT NULL,
  rpt_mod TEXT NULL
)

CREATE TABLE "metric_quantile" (
	"niche_set" TEXT NOT NULL,
	"dist_ses_id"	TEXT NOT NULL,
	"stat_ses_id"	TEXT NOT NULL,
	"q_spec"	REAL NOT NULL,
	"q_nestedness"	REAL NOT NULL,
	"q_clust_w"	REAL NOT NULL
);

CREATE TABLE "metric_ci" (
	"ses_id"	TEXT NOT NULL,
	"niche_set" TEXT NOT NULL,
	"metric"	TEXT NOT NULL,
	"ci_low"	REAL NOT NULL,
	"ci_high"	REAL NOT NULL
);