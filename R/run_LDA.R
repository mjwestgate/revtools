# function to run the topic model  requested by the user - used by plot_bibliography
run_LDA<-function(
	x,  # DTM
	topic.model="lda", n.topics=6, iter=2000, ...)
	{
	LDA.control<-list(iter=iter)
	switch(topic.model,
		"ctm"={topicmodels::CTM(x, k= n.topics)},
		"lda"={topicmodels::LDA(x, k= n.topics, method="Gibbs",  control = LDA.control)})
	}