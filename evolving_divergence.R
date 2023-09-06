#############################
# Code to run analyses in Karjus & Cuskley, 
# "Evolving linguistic divergence on polarizing social media"
# Note that the data is unfortunately not public :( as we cannot
# risk violating Twitter/X TOS, given the political content.
# This code can still be useful to understand our analysis,
# as well as to run the same analyses on similar datasets mined from twitter/x
# and adopted to data mined from other platforms.
#############################


source("twitter_scripts.R")  # point to scipts file
corpusfolder ="/twitter"     # define path to corpus

#### Cleaning ####
load(file.path(corpusfolder, "followerminer/fixed", "tweets.RData"))


tweets = tweetfilter_spam(tweets)


library(spacyr); spacy_initialize()  # this is initialized here not in scripts.R as it conflicts with reticulate; the two should be run in separate R sessions.
lem2 = tweetlemmatizer(tweets$text, news=tweets$status_id[tweets$side=="news"])
multiword = attr(lem2, "multiword")
extracorpus = attr(lem2, "extras") # extract for later
attr(lem2, "extras") = NULL
tw = tweets %>% filter(status_id %in% names(lem2))
dtm0 = lem2 %>% dfm(remove_padding = T) %>% 
  dfm_group(groups=tw$user_id) %>% 
  dfm_trim(min_termfreq = 100)
# proportion of word users by word occurrence, log scale:
wprop = log(colSums(dfm_weight(dtm0, "boolean")))/log(colSums(dtm0))

# filter out users who mostly use very specific, idiosyncratic language (words only used by them or them and a few others, doesn't reflect population), or uses basically only stopwords
# removes about 100 spammy accounts basically
dtmu = t(t( dfm_weight(dtm0, "prop") )*wprop) %>% rowSums()
dtmu %>% sort() %>% plot() # 0.76
# load(file.path(corpusfolder, "followerminer/fixed", "allusers.RData"))
okusers = names(dtmu[dtmu>0.76]) 
length(okusers) # 10986, gets rid of a handful of weird/spammy users, -171681 words (the lowest score is 200 tweets from somebody regularly spamming the same religious hashtags.
dtm0 = dtm0 %>% dfm_trim(min_docfreq = 50) %>% .[okusers,]
dtm0 = dtm0[rowSums(dtm0)>=20,]
dim(dtm0); sum(dtm0) # 10471  9201, 19,249,336
tf = dfm_tfidf(dtm0)
wusers = colSums(dfm_weight(dtm0, "boolean"))



####  Word frequencies ####

freqs = dofreqs(lem2, tw, okusers, wprop, wusers) %>% 
  filter(ok) %>% select(-ok)

freqs1 = freqs %>% filter(#notname &
  (term_count_l >= 200 | term_count_r >= 200 | (term_count_l+term_count_r)>=300) & 
    nusers>200 &   # ratio filter already in preprocessing
    fnlm<8.21    # leave super frequent stuff out of plot, just stopwords, boring
) %>% 
  mutate(nusersp = nusers/length(okusers)*100) %>% 
  filter(nusersp<40) %>% 
  #mutate(sizemult = log2dif_doc*(1/nusers) ) %>% 
  mutate(sizemult = scale(abs(log2dif_doc))[,1] + (scale(nusers)[,1]*50)  ) %>% 
  mutate(big=case_when(
    (abs(log2dif_doc)>1.9 & term!="🇺🇸") | term=="😂"  ~ T,  # filter example labels a bit
    T~F
  )
  ) %>% 
  mutate(term=case_when(term=="nigga"~"n*gga", T~term)) %>% 
  mutate(term=gsub("_","\n",term)) %>% 
  filter(term!="🥲") 
#dim(freqs1)

comparisonplot(freqs1, xvar="log2dif_doc", ymx=40)



library(reticulate) # do not load in the same R session as spacyr
use_condaenv('r-reticulate', required = T )
gensim = import("gensim")  # install if not present

freqs2 = freqs %>% filter(term_count_l >= 100 & term_count_r >= 100 & 
                            (term_count_l >= 200 | term_count_r >= 200) & 
                            nusers>200) # additional constraints to do sem comparison
voc = freqs2$term #  3613
#
leml = lem2[tw$user_id %in% okusers & tw$side == "left"]  %>% as.list() %>% unname() #%>% .[1:100]
lemr = lem2[tw$user_id %in% okusers & tw$side == "right"] %>% as.list() %>% unname() #%>% .[1:100]

# to get high average alignment, moderate dim, lower epochs, high negative sampling seems to work best.
# increasing mincount is bad, too many epochs bad (overfits?).
py_run_string(
  "
from gensim.models import FastText
import numpy as np
from gensim.test.utils import get_tmpfile

modell = FastText(sentences=r.leml, vector_size=50, window=5, workers=8, min_count=5, negative=20, epochs=5)

modelr = FastText(sentences=r.lemr, vector_size=50, window=5, workers=8, min_count=5, negative=20, epochs=5)
"
)

vecl = py$modell$wv$vectors_for_all(voc)$vectors %>% normalize("l2")
vecr = py$modelr$wv$vectors_for_all(voc)$vectors %>% normalize("l2")
#vecl = scale(vecl, T, F) # mean-centering 
#vecr = scale(vecr, T, F)
vecl2 = orthprocr(X=vecl, Z=vecr)$XQ
summary(psim2(vecl2, vecr)) 
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.3583  0.8803  0.9165  0.8977  0.9394  0.9813 
#0.3046  0.8780  0.9161  0.8965  0.9389  0.9824 
rownames(vecl)=voc
rownames(vecl2)=voc
rownames(vecr)=voc



#### Semantics for topic model ####

library(reticulate); use_condaenv('r-reticulate', required = T )

freqs3 = freqs %>% filter(nusers>200) 
voc = freqs3$term #  5420
lem3 = lem2[tw$user_id %in% okusers]  %>% as.list() %>% unname()

py_run_string(
  "
from gensim.models import FastText
import numpy as np
from gensim.test.utils import get_tmpfile

modell = FastText(sentences=r.lem3, vector_size=50, window=5, workers=3, min_count=5, negative=20, epochs=10)
"
)
vecs = py$modell$wv$vectors_for_all(voc)$vectors 
rownames(vecs)=voc
ud = umap.defaults;ud$min_dist=0.8
umsem = umap(vecs, config = ud)$layout
fullvecs = py$modell$wv$vectors_for_all( py$modell$wv$index_to_key )$vectors 
rownames(fullvecs)=py$modell$wv$index_to_key



##### Topics ####
# doc2vec topic model 
library(doc2vec)

tweetframe = lem2 %>% as.list() %>% .[tw$user_id %in% okusers] %>% 
  {data.frame(doc_id=names(.), 
              text=sapply(., paste, collapse=" "), 
              user = tw$user_id[tw$user_id %in% okusers], 
              side=tw$side[tw$user_id %in% okusers])}
rownames(tweetframe)=tweetframe$doc_id
twvecs = paragraph2vec(tweetframe[, 1:2], threads=8, embeddings=fullvecs, dim=50, iter=20) %>% 
  as.matrix(which = "docs")


library(reticulate); use_condaenv('r-reticulate', required = T )
twumap = umap(twvecs, method = "umap-learn")$layout
twumap = cbind(tweetframe, twumap %>% as.data.frame())


twumaptmp = twumap2
eps = c(0.006, 0.008, 0.01)
res = tibble()
for(i in seq_along(eps)){
  twtmp = twumaptmp %>% ungroup %>% select(V1, V2) %>% as.matrix()
  tmp = dbscan(twtmp, minPts = 4, eps = eps[i])
  cltmp = tmp$cluster %>% as.character() 
  twumaptmp$cl = cltmp %>% paste0(.,"_",i)
  tcl = table(tmp$cluster)
  res=rbind(res, twumaptmp[which(cltmp %in% 
                                   names(tcl)[tcl >= 1000 & tcl <= 100000 & names(tcl)!="0" ]),] %>% mutate)
  twumaptmp = twumaptmp[which(cltmp %in% 
                                names(tcl)[tcl < 1000 | tcl > 100000 | names(tcl)=="0" ] ),]
}
nrow(res)
twumap3 = rbind(res, # %>% mutate(cl=case_when(cl %in% c("57_1","103_3")~NA_character_, T~cl ) ), 
                twumap2 %>% 
                  filter(!(doc_id %in% res$doc_id)) %>% mutate(cl=NA_character_) 
) %>% left_join(tw %>% select(status_id, text) %>% 
                  rename(fulltext=text, doc_id=status_id), by="doc_id")

# keywords
twmat = twumap3 %>%  pull(text) %>% tokens %>% 
  dfm() %>% 
  dfm_trim(min_termfreq = 500) %>% 
  dfm_remove( max_nchar=15, 
              pattern="[\"'°,.;:=?!¤%&/()-]|^[a-z]{1,2}$|_[ts]$", valuetype="regex") %>% 
  dfm_remove(c(stopwords("spa"), stopwords("en")))  %>% 
  dfm_weight("boolean") # otherwise emojis dominate
docvars(twmat, field="doc_id") = twumap3$doc_id
docvars(twmat, field="cl") = twumap3$cl
twmat=twmat[!is.na(twumap3$cl), ]
twmattf = dfm_group(twmat,cl) %>% dfm_tfidf(force=T)
tops = topfeatures(twmattf, groups = cl, n = 20)

twkeywordmap = twumap3 %>% filter(!is.na(cl)) %>% 
  mutate(right=as.numeric(side=="right")) %>% 
  group_by(cl) %>% 
  summarise(V1=mean(V1), V2=mean(V2), n=round(log2(n())-8), side2=sum(right)/n()) %>% 
  mutate(keywords = tops[cl]) %>%  
  unnest_longer(keywords ) %>% 
  group_by(cl) %>%  arrange(-keywords, .by_group = T) %>%  mutate(ind = 1:n()) %>% ungroup() %>% 
  group_by(keywords_id) %>% arrange(ind, .by_group = T) %>% slice(1) %>% ungroup() %>% 
  arrange(cl) %>% arrange(-keywords, .by_group = T) %>% 
  group_by(cl) %>% slice(1:n[1]) %>% 
  mutate(size=rescale(n():1, c(0.1,1)) + n[1]) %>% 
  mutate(size=case_when(nchar(keywords_id)==1 ~ size*0.3, T~size)) %>%  # emoji print size mod
  mutate(keywords_id=gsub("_","\n",keywords_id)) %>%    # phrases
  mutate(size=case_when(cl %in% c("57_1", "100_3")~size*0.7, T~size)) # corner too big

g=ggplot(twumap3
         , aes( -V1, V2))+ 
  geom_vline(xintercept=mean(c(min(-twumap3$V1), max(-twumap3$V1))), size=0.1, color="gray70" )+
  geom_hline(yintercept=mean(c(min(twumap3$V2), max(twumap3$V2))), size=0.1,  color="gray70" )+
  geom_point(aes(fill=side), alpha=0.4, shape=21, color="transparent", stroke=0, size=0.2)+
  geom_text_repel(aes(label=keywords_id, size=size, color=side2), data=twkeywordmap,  box.padding = 0, min.segment.length = 9999, max.time = 5, max.overlaps = 1000000, direction = "both", segment.size=unit(0,"mm"), segment.alpha=0, lineheight=0.45)+
  theme_bw()+
  scale_size(range=c(1.4,3.4))+
  coord_cartesian(expand = F)+
  labs(title="(a) 1,483,385 tweets by 10,986 US users; February-September 2021 (blue: from left-aligned users; red: right-aligned)")+
  #scale_y_continuous(expand = expansion(add=0.1))+
  #scale_x_continuous(expand = expansion(add=0.1))+
  theme(
    plot.title = element_text(size=7, margin=margin(1,0,1,0)),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.ticks.length = unit(0, "null"),
    plot.margin = margin(1,1,1,1, unit="pt"),
    panel.spacing = unit(0, "pt"),
    legend.position = "none",
    #legend.direction="horizontal",
    legend.key.width=unit(30,"pt"),
    legend.background = element_rect(color=NA, fill=NA),
    legend.box.just = "right",
    legend.title = element_blank()
  )+
  #scale_fill_manual(values=c("#4A6FE3", "#DB6581"))+
  scale_fill_manual(values=c("#007bff", "#e85d5d"))+
  scale_color_gradientn(colors=diverging_hcl(11, palette = "Blue-Red") %>% .[c(1,2,3,10,11)] %>% {.[3]="gray40";.} , limits=c(0.1,0.9))+
  NULL



#### Sentiment ####

twsent = tw %>% filter(user_id %in% okusers) %>% 
  mutate(text=gsub("🔥", "", text)) # classed as neg in vader, often leads to weird results
tmp=twsent$text
py_run_string(
  "
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
analyzer = SentimentIntensityAnalyzer()
"
)
# py_run_string("print(analyzer.polarity_scores(r.tmp))")
py_run_string(
  "
res=[]
neu=[]
for s in r.tmp:
    x = analyzer.polarity_scores(s)
    res.append(x['compound'])
    neu.append(x['neu'])
"
)
twsent$sent = py$res; twsent$neu = py$neu; rm(tmp)
twsent = twsent %>% left_join(allusers %>% select(user_id, followers_count), by = "user_id")



#### Semantics  ####

lex = read.table("stim_lex.txt", sep="\t", encoding="UTF-8", quote='"', na.strings = "NA", header = 1)
freqs2 = freqs %>% filter( term_count_l >= 100 & term_count_r >= 100 & 
                             (term_count_l >= 200 | term_count_r >= 200) & 
                             nusers>200, !grepl("^[[:punct:]]+$", term)
) %>% 
  mutate(semdif = 1- (psim2(vecl2, vecr)[term])) %>% # 1-cosinesim = distance
  mutate(sizemult=1) %>% 
  mutate(nusersp = nusers/length(okusers)*100) %>% 
  filter(nusersp<80) %>% 
  #mutate(sizemult = log2dif_doc*(1/nusers) ) %>% 
  #mutate(sizemult = scale(abs(log2dif_doc))[,1] + (scale(nusers)[,1]*50)  ) %>% 
  mutate(big=case_when(
    (semdif>0.4 | term %in% lex$word) & term!="🇺🇸"  ~ T,
    T~F
  )
  ) %>% 
  mutate(term=case_when(term=="nigga"~"n*gga", T~term)) %>% 
  mutate(term=gsub("_","\n",term)) %>% # for plotting
  filter(term!="🥲") 

comparisonplot(freqs2, xvar="semdif", ymx=34)+theme(plot.margin = margin(0,4,0,0))
