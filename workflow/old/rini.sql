select ns.ses_id, ns.rep, n.niche_set, ns.niche_name, ns.niche_vol, nss.niche_set_vol, ns.niche_vol/nss.niche_set_vol as rini
from niche_stats ns
inner join niche n
on ns.niche_name = n.niche_name
inner join niche_set_stats nss
on ns.ses_id = nss.ses_id and ns.rep = nss.rep and n.niche_set = nss.niche_set
where ns.ses_id = 'full_hvs' 
order by ns.ses_id, ns.rep, n.niche_set, ns.niche_name

and ns.rep=1