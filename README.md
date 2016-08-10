# log_statsbars_R

**purpose:** drawing stats bars in R on a log scale with categorical data on the X axis

the function logscale_sigbars_generator takes in the Y values for the minimum and maximum location to plot the significance bars as well as how many bars to make and the size and spacing of them
it returns a data frame with the positions of the lines requred to draw the bars in R
this matrix can then be used to draw the sig bars (in my example I use ggplot2)

## [Example use](http://htmlpreview.github.io/?https://github.com/danwchan/log_statsbars_R/blob/master/log_sigbar_example.html)

## Future Directions

1. automatically add stats text based on an imported dataframe of statistics
2. be able to call the script interactively from the commandline
 
