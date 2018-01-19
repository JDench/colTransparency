# This allows the opacity of colours to be adjusted, it's inspried from mage's blog
# who wrote the add.alpha function.
colTransparency <- function(func_cols, func_opacity=1){
	# After extracting the RGB balues of a colour we change the values based on the opacity.
  return( apply(sapply(func_cols, col2rgb)/255, MARGIN = 2,function(x){
  				rgb(red = x[1], green = x[2], blue = x[3], alpha= func_opacity) 
  			}) )
                        
}