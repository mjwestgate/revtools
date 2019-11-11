# function to run a topic model with specified parameters
run_topic_model <- function(
	dtm,
	type = "lda",
	n_topics = 5,
	iterations = 2000
){
	LDA_control <- list(
    iter = iterations,
    burnin = iterations * 0.1
  )
	switch(type,
		"ctm" = {
      topicmodels::CTM(
        dtm,
        k = n_topics
      )
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