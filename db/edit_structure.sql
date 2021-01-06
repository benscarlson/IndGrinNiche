
---- Don't need to do these if creating the database from scratch ----

alter table niche_stats
add column spec real as (1-rini)

alter table niche_set_stats
add column spec real as (1-rini)

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