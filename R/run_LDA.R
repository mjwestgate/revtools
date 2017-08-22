# function to run the topic model  requested by the user - used by plot_bibliography
run_LDA<-function(
	x,  # DTM
	topic.model="LDA", n.topics=6, iter=2000, ...)
	{
	LDA.control<-list(iter=iter)
	switch(topic.model,
		"CTM"={topicmodels::CTM(x, k= n.topics)},
		"LDA"={topicmodels::LDA(x, k= n.topics, method="Gibbs",  control = LDA.control)})
	}