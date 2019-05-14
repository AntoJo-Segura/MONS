library(dplyr)
e <- 2.71828182846
g <- function(x){e^(-(x)^2)}
pulse <- function(x){-g(x-1)+g(x+1)}
seq(-10,10,0.1) %>% 
  (function(x){
      plot( x, Re(fft(pulse(x))), type = "l" )
      lines(x, Im(fft(pulse(x))), col = 2)
  })

seq(-10,10,0.1) %>% 
  (function(x){
    plot( x, fft((fft(pulse(x))), inverse = TRUE), type = "l" )
  })

con.p <- function(f,ft){
  seq(-10,10,0.1) %>% 
    (function(x){
      plot( x, convolve(f(x),ft(x)), type = "l" )
    })
}

con <- function(f,ft){
  (function(x){ convolve(f(x),ft(x))})
}

#example, fail to get inverse because of 0 results
const <- (function(x){rep(1,length(x))})
 
( 
    seq(-10,10,0.1) %>% fft(con(pulse, const) ) 
)/(
    fft(pulse(seq(-10,10,0.1))) * fft(const(seq(-10,10,0.1)))
)


fft( 
    (seq(-10,10,0.1) %>% fft(con(pulse, const) ) )/fft(pulse(seq(-10,10,0.1))) 
, inverse = TRUE )



#looks like inverse but there are many zeros
plot(seq(-10,10,0.1),seq(-10,10,0.1) %>% fft(con(pulse, const) ) )

plot(seq(-10,10,0.1), fft(pulse(seq(-10,10,0.1))) * fft(const(seq(-10,10,0.1))) )
#end example


#used into con.ft maybe usefull
fft.f <- function(f){
  (function(x){fft(f(x))})
}
#get function that f was convoluted with to get r
con.ft <- function(f, r){
  (function(x){
         fft( 
              ( con( fft.f(f), fft.f(r) )(x)/ f(x) )
         , inverse = TRUE)
  })
}


con.ft(gauss,pulse)(domain)


domain <- seq(-10,10,0.1)
gauss <- (function(x){exp(-x^2)})


con.p(pulse, con.ft(gauss,pulse) )(domain) # buena pinta para ft

con.p(con(pulse, con.ft(gauss,pulse) ), pulse)(domain) #pero no hay forma de volver a la gausiana :(
