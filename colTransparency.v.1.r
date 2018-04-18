# This allows the opacity, saturation and hue of colours to be adjusted.  The purpose is to allow
# a range of basic named colours to be dynamically manipulted into altered vectors of the colour. 
colTransparency <- function(func_cols, func_opacity=1, func_scaleSaturation = NA, func_scaleValue = NA){
	# In case the user has passed scaling values outside of the 0-1 range we simply return them to being values of 1
	funcParms = c("Saturation"= func_scaleSaturation,
					"Value"= func_scaleValue,
					"Opacity"= func_opacity)
	# We check that the opacity value is within the range of 0-1				
	for(thisParm in names(funcParms)){
		# If the user has not defined anything, then let it pass....
		if(!is.na(funcParms[thisParm])){
			if(funcParms[thisParm] > 1){
				funcParms[thisParm] <- 1
			} else if (funcParms[thisParm] < 0){
				funcParms[thisParm] <- 0
			}
		}
	}
	
	# After extracting the RGB balues of a colour we change the values based on the opacity.
  	return( apply(rgb2hsv(sapply(func_cols, col2rgb)), MARGIN = 2,function(x){
  				hsv(h = x[1], 
  					s = if(is.na(funcParms["Saturation"])){
  							x[2]
  						} else {
  							funcParms["Saturation"]
  						}, 
  					v = if(is.na(funcParms["Value"])){
  							x[3]
  						} else {
  							funcParms["Value"]
  						}, 
  					alpha= funcParms["Opacity"]) 
  			}) )
}
