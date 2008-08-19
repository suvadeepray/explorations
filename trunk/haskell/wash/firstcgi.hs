module Main where

import CGI hiding (div,head,map,span)

main = 
    run mainCGI
   

mainCGI =
    ask <html>
          <head><title>Hello World</title></head>
          <body>
          <h1>Hello World</h1>
          </body>
        </html>

    