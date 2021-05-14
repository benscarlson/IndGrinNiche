#!/usr/bin/env Rscript --vanilla
# chmod 744 script_template.r #Use to make executable

# This script implements the breezy philosophy: github.com/benscarlson/breezy

# ==== Breezy setup ====

'
Template

Usage:
env_dist.r <dat> <out> <env> [-t] [--seed=<seed>]
env_dist.r (-h | --help)

Options:
-h --help     Show this screen.
-v --version     Show version.
-s --seed=<seed>  Random seed. Defaults to 5326 if not passed
-t --test         Indicates script is a test run, will not save output parameters or commit to git
' -> doc

isAbsolute <- function(path) {
  grepl("^(/|[A-Za-z]:|\\\\|~)", path)
}

#---- Input Parameters ----#
if(interactive()) {
  library(here)

  .wd <- '~/projects/ms1/analysis/rev2/fig_env_dist_poc'
  .seed <- NULL
  .test <- TRUE
  rd <- here::here
  
  .env <- 'pct_tree'
  
  #.datPF <- file.path(.wd,'data/dat.csv')
  .datPF <- '~/projects/ms1/data/derived/obs_anno.csv'
  .outPF <- file.path(.wd,'figs',glue('{.env}.pdf'))
  
} else {
  library(docopt)
  library(rprojroot)

  ag <- docopt(doc, version = '0.1\n')
  .wd <- getwd()
  .script <-  thisfile()
  .seed <- ag$seed
  .test <- as.logical(ag$test)
  rd <- is_rstudio_project$make_fix_file(.script)
  
  .env <- ag$env
  #.list <- trimws(unlist(strsplit(ag$list,',')))
  .datPF <- ifelse(isAbsolute(ag$dat),ag$dat,file.path(.wd,ag$dat))
  .outPF <- ifelse(isAbsolute(ag$out),ag$out,file.path(.wd,ag$out))
}

#---- Initialize Environment ----#
.seed <- ifelse(is.null(.seed),5326,as.numeric(.seed))

set.seed(.seed)
t0 <- Sys.time()

source(rd('src/startup.r'))

#Source all files in the auto load funs directory
list.files(rd('src/funs/auto'),full.names=TRUE) %>%
  walk(source)

theme_set(theme_ms)

#---- Local parameters ----#

#---- Load control files ----#
nsets <- read_csv(file.path(.wd,'ctfs/niche_sets.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run)
niches <- read_csv(file.path(.wd,'ctfs/niches.csv'),col_types=cols()) %>% 
  filter(as.logical(run)) %>% select(-run) %>%
  inner_join(nsets %>% select(niche_set),by='niche_set')

envinfo <- read_csv(file.path(.wd,'envs.csv'),col_types=cols()) %>%
  filter(label==!!.env)

#---- Load data ----#
message('Loading data...')
tic()
dat0 <- read_csv(.datPF,col_types=cols()) %>%
  inner_join(niches %>% select(niche_set,niche_name),by='niche_name')
toc()
#====

#---- Perform analysis ----#

dat <- dat0 %>%
  mutate(niche_set=factor(niche_set,levels=nsets$niche_set,labels=nsets$label))
  #%>% filter(obs) #using obs_anno instead of obsbg_anno, so don't need to filter

envlab <- envinfo$label_long
if(!is.na(envinfo$units)) envlab <- glue('{envlab} ({envinfo$units})')

env <- quo(!!sym(.env))

p <- ggplot(dat,aes(x=!!env,y=..scaled..,color=niche_name)) +
  geom_density() +
  theme(
    axis.text=element_text(size=8),
    strip.text = element_text(size=13),
    strip.background = element_blank()) +
  guides(color=FALSE) +
  labs(title=envinfo$label_long,x=envlab,y='Relative density') +
  facet_wrap(vars(niche_set),scales='free',ncol=4)

# if(.env %in% c('pct_tree','pct_bare')) {
#   p <- p + xlim(NA,10)
# }
#---- Save output ---#
dir.create(dirname(.outPF),recursive=TRUE,showWarnings=FALSE)

h=6; w=10
if(fext(.outPF)=='pdf') {
  ggsave(.outPF,plot=p,height=h,width=w,device=cairo_pdf) #save pdf
} else if(fext(.outPF)=='png') {
  ggsave(.outPF,plot=p,height=h,width=w,type='cairo')
}


#---- Finalize script ----#

if(!.test) {
  library(git2r)
  library(uuid)
  
  .runid <- UUIDgenerate()
  .parPF <- file.path(.wd,"run_params.csv")
  
  #Update repo and pull out commit sha
  repo <- repository(rd('src'))
  
  rstat <- status(repo)
  if(length(rstat$staged) + 
     length(rstat$unstaged) + 
     length(rstat$untracked) > 0) {
    add(repo,'.')
    commit(repo, glue('script auto update. runid: {.runid}'))
  }
  
  
  .git_sha <- sha(repository_head(repo))
  
  #Save all parameters to csv for reproducibility
  #TODO: write this to a workflow database instead
  saveParams(.parPF)
}

# if(.test) {
#   message('Rolling back transaction because this is a test run.')
#   dbRollback(db)
# } else {
#   dbCommit(db)
# }
# 
# dbDisconnect(db)

message(glue('Script complete in {diffmin(t0)} minutes'))