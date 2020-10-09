library( readr )
library( profvis ) # for profiling

# Read in data (see https://astrostatistics.psu.edu/datasets/SDSS_quasar.html)
# you can try thisline, but if it doesn't work, then download the data file and use the next (commented) line instead
SDSS_quasar <- read_table("https://astrostatistics.psu.edu/datasets/SDSS_quasar.dat")

# SDSS_quasar <- read_table("SDSS_quasar.dat")

# -9 are missing values in X-ray, so put those in
SDSS_quasar[ SDSS_quasar == -9 ] <- NA_real_
SDSS_quasar <- as.data.frame( SDSS_quasar )

# Scatterplot
plot( `X-ray` ~ i_mag, data = SDSS_quasar )

# Estimate regression parameters for X-ray versus optical
summary( lm( `X-ray` ~ i_mag, data = SDSS_quasar ) )


# Write function that returns regression parameters when given data
get_coefs <- function( x ){
  cc <- coef( lm( `X-ray` ~ i_mag, data = x ) )
data.frame( `(Intercept)` = cc[1], i_mag = cc[2], row.names = "1", check.names = FALSE )
}
get_coefs( SDSS_quasar )

# Write function that does a bootstrap, collecting results using c()
boot_coefs <- function( x, n = 100 ){
  # object to store coefficients
  cf <- data.frame( `(Intercept)` = numeric(0), i_mag = numeric(0), check.names = FALSE )
  for( i in 1:n ){
    # resample data
    xx <- x[ sample.int( nrow(x) ) ]  ### THIS WILL THROW AN ERROR BECAUSE WRONG DIMS. ALSO, NEEDS replace = TRUE.
    # get coefficients
    cf <- rbind( cf, get_coefs( x ) ) ### THIS IS A NON-ERROR MISTAKE: SHOULD BE get_coefs( xx ) 
  }
cf
}

# pass the SDSS_quasar data to the function that does the bootstrap regression
boot_coefs( SDSS_quasar, n = 10 )

# Debug using traceback()
traceback()

# Debug using browser()
# Put a browser() in there somewhere

# Debug using options( error = recover )
options( error = recover )
# You can reset this option using options( error = NULL )

# Debug using cat() or print()
# Try inserting after the line cf <- rbind( cf, get_coefs( x ) )


# Profile code using profvis
profvis(boot_coefs( SDSS_quasar, n = 500 ))

# Write function that does a bootstrap, collecting results in a pre-defined vector
boot_coefs_faster <- function( x, n = 100 ){
  # object to store coefficients
  cf <- data.frame( `(Intercept)` = numeric(n), i_mag = numeric(n), check.names = FALSE )
  for( i in 1:n ){
    # resample data
    xx <- x[ sample.int( nrow(x), replace = TRUE ), ]
    # get coefficients
    cf[i,] <- get_coefs( xx )
  }
cf
}
boot_coefs_faster( SDSS_quasar, n = 10 )

# Profile code again
profvis(boot_coefs_faster( SDSS_quasar, n = 500 ))



# Write function that takes entire dataset, does bootstrap, and creates a ggplot of results
boot_plot <- function( x, n = 100 ){
  require( ggplot2 )
  # get bootstrapped coefficients
  bcf <- boot_coefs_faster( x, n )
  # create ggplot that shows the range of possible regression lines
  gg <- ggplot( SDSS_quasar, aes( x = i_mag, y = `X-ray` ) ) +
    geom_point() +
  # add the bootstrapped regression lines
    geom_abline( aes( intercept = `(Intercept)`, slope = i_mag ), 
                 data = bcf, alpha = 0.05 ) +
  # add best-fit regression line
    geom_smooth( method = lm, se = FALSE, size = 1.2 )
  
gg  
}

bgg <- boot_plot( SDSS_quasar ) + theme_bw()

# For fun...
library( plotly )
ggplotly( bgg )




