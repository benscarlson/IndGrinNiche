--#------------------------#--
--# Manual updates to data #--
--#------------------------#--

-- update old volumes because they didn't set distance.factor correctly
update niche_set_vol set hv_job_name='5axes2000pts1_old' where hv_job_name='5axes2000pts1'

-- Need to have jaccard in pairwise table. In the future, update hv_pairwise to include this calculation
update pairwise
set jaccard = (hv_intr_vol/hv_union_vol)
where hv_job = '5axes2000pts1'

-- 2020-11-16 Clean up old analysis runs
delete from pairwise where hv_job='5axes2000pts1_old'
delete from niche_set_stats where hv_job in ('4axes1000pts', '5axes2000pts1_old')
delete from indiv_stats where hv_job in ('4axes1000pts', '4axes2000pts1', '5axes2000pts1_old')

--#-----------------------------#--
--# Manual updates to structure #--
--#-----------------------------#--

--# Used to alter database after initial creation (create_db.sh)
--# All updates here should be incorporated into create_db.sh, so subequent databases won't need to run alter_db.sql

ALTER TABLE pairwise ADD nestedness REAL;
ALTER TABLE pairwise ADD hv_union_vol REAL;
ALTER TABLE pairwise ADD hv_unique1_vol REAL;
ALTER TABLE pairwise ADD hv_unique2_vol REAL;

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

-- 2020-11-16 Make updates to tables holding niche metrics data
alter table indiv_stats rename to niche_stats
alter table niche_stats add rep INTEGER
-- use DB Browser to rename to niche_stats_id, niche_vol, ses_id, order columns

alter table niche_set_stats add rep INTEGER
-- use DB Browser to rename to ses_id, order columns

alter table pairwise add rep INTEGER
-- use DB Browser to rename to ses_id, order columns

ALTER TABLE employees
  RENAME TO staff;