# This allows the opacity, saturation and hue of colours to be adjusted.  The purpose is to allow
# a range of basic named colours to be dynamically manipulted into altered vectors of the colour. 
colTransparency <- function(func_cols, func_opacity=1, func_scaleSaturation = NA, func_scaleValue = NA){
	# In case the user has passed scaling values outside of the 0-1 range we simply return them to being values of 1
	funcParms = list("Saturation"= func_scaleSaturation,
					"Value"= func_scaleValue,
					"Opacity"= func_opacity)
	# We check that the opacity value is within the range of 0-1				
	for(thisParm in names(funcParms)){
		# If the user has not defined anything, then let it pass....
		if(all(!is.na(funcParms[[thisParm]]))){
			if(any(funcParms[[thisParm]] > 1)){
				funcParms[[thisParm]][which(funcParms[thisParm] > 1)] <- 1
			} else if (any(funcParms[[thisParm]] < 0)){
				funcParms[[thisParm]][which(funcParms[thisParm] < 0)] <- 0
			}
		}
	}
	
	# After extracting the RGB balues of a colour we change the values based on the opacity.
  	tmpReturn <- rgb2hsv(sapply(func_cols, col2rgb))
  	return( sapply(1:ncol(tmpReturn),function(x){
	  				hsv(h = tmpReturn[1,x], 
	  					s = if(any(is.na(funcParms[["Saturation"]]))){
	  							tmpReturn[2,x]
	  						} else {
	  							funcParms[["Saturation"]]
	  						}, 
	  					v = if(any(is.na(funcParms[["Value"]]))){
	  							tmpReturn[3,x]
	  						} else {
	  							funcParms[["Value"]]
	  						}, 
	  					alpha= funcParms[["Opacity"]]) 
	  			}) )
}
