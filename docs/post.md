## Sending Post

### curlPost

### curlPostString
with Form data, return with curl code and response string

    curlPostString "http://127.0.0.1:5000/login" [CurlPort 5000,CurlPostFields ["A=1","B=2"]] []
    
with Json data, return with curl code and response string

    curlPostString "http://127.0.0.1:5000/login" [CurlPort 5000, CurlHttpHeaders ["Content-Type:application/json"],CurlPostFields ["{\"a\":1}"] ] 
