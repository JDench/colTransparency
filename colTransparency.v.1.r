# This allows the opacity, saturation and hue of colours to be adjusted.  The purpose is to allow
# a range of basic named colours to be dynamically manipulted into altered vectors of the colour. 
colTransparency <- function(func_cols, func_opacity=1, func_scaleSaturation = 1, func_scaleValue = 1){
	# In case the user has passed scaling values outside of the 0-1 range we simply return them to being values of 1
	funcParms = c("Saturation"= func_scaleSaturation,
					"Value"= func_scaleValue,
					"Opacity"= func_opacity)
	for(thisParm in names(funcParms)){
		if(funcParms[thisParm] > 1 || funcParms[thisParm] < 0){
			funcParms[thisParm] <- 1
		}	
	}
	
	# After extracting the RGB balues of a colour we change the values based on the opacity.
  	return( apply(rgb2hsv(sapply(func_cols, col2rgb)), MARGIN = 2,function(x){
  				hsv(h = x[1], 
  					s = x[2] * funcParms["Saturation"], 
  					v = x[3] * funcParms["Value"], alpha= funcParms["Opacity"]) 
  			}) )
}
