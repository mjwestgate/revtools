plot1_3D<-function(input_info, color, pointsize=10, height=400){
	p<-plot_ly(input_info, #[which(input_info$display), ], 
		x=~Axis1, y=~Axis2, z=~Axis3, 
		type="scatter3d", 
		mode="markers",	
		marker=list(
			size=pointsize,
			color= color), # add_color(input_info$topic[which(input_info$display)], palette)),
		hoverinfo="text",
		hoverlabel=list(
			bgcolor=grey(0.9), 
			bordercolor=grey(0.9),
			namelength=200, 
			font=list(color="black")
			# opacity=0.7
		),
		text= ~ caption,
		height=height,
		source="main_plot"
		) %>%
	add_markers() %>%
	layout(
		showlegend=FALSE,
		scene=list(
			xaxis=list(showticklabels=FALSE),
			yaxis=list(showticklabels=FALSE),
			zaxis=list(showticklabels=FALSE))
		)
	p
}


plot1_2D<-function(input_info, color, pointsize=10, height=400){
	p<-plot_ly(input_info, # [which(input_info$display), ], 
		x=~Axis1, y=~Axis2,
		type="scatter", 
		mode="markers",	
		marker=list(
			size=pointsize,
			color=color), # add_color(input_info$topic[which(input_info$display)], palette)),
		hoverinfo="text",
		hoverlabel=list(bgcolor=grey(0.9), bordercolor=grey(0.9),
			namelength=200, font=list(color="black")),
		text= ~ caption,
		height=height,
		source="main_plot"
		) %>%
	add_markers() %>%
	layout(
		showlegend=FALSE,
		xaxis=list(showticklabels=FALSE),
		yaxis=list(showticklabels=FALSE)
		)
	p
}


plot2_3D<-function(input_info, color, pointsize=10, height=400){
	p<-plot_ly(input_info, 
		x=~Axis1, y=~Axis2, z=~Axis3, 
		type="scatter3d", 
		mode="markers",	
		marker=list(
			size=pointsize,
			color=color), # add_color(input_info$topic, palette)),
		hoverinfo="text",
		hoverlabel=list(bgcolor=grey(0.9), bordercolor=grey(0.9),
			namelength=200, font=list(color="black")),
		text= ~ caption,
		height=height,
		source="main_plot"
		) %>%
	add_markers() %>%
	layout(
		showlegend=FALSE,
		scene=list(
			xaxis=list(showticklabels=FALSE),
			yaxis=list(showticklabels=FALSE),
			zaxis=list(showticklabels=FALSE))
		)
	p
}


plot2_2D<-function(input_info, color, pointsize=10, height=400){
	p<-plot_ly(input_info, 
		x=~Axis1, y=~Axis2,
		type="scatter", 
		mode="markers",	
		marker=list(
			size=pointsize,
			color=color), # add_color(input_info$topic, palette)),
		hoverinfo="text",
		hoverlabel=list(bgcolor=grey(0.9), bordercolor=grey(0.9),
			namelength=200, font=list(color="black")),
		text= ~ caption,
		height=height,
		source="main_plot"
		) %>%
	add_markers() %>%
	layout(
		showlegend=FALSE,
		xaxis=list(showticklabels=FALSE),
		yaxis=list(showticklabels=FALSE)
		)
	p
}