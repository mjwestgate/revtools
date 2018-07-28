# function to run a topic model with specified parameters
run_LDA<-function(
	dtm,
	topic_model = "lda",
	n_topics = 5,
	iterations = 2000
	){
	LDA_control <- list(
    iter = iterations
  )
	switch(topic_model,
		"ctm" = { topicmodels::CTM(
      dtm,
      k = n_topics)
    },
		"lda" = {
      topicmodels::LDA(
        dtm,
        k = n_topics,
        method = "Gibbs",
        control = LDA_control
      )
    }
  )
	}