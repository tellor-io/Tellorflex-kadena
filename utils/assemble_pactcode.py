from base64_encoding import base64_url_encode
from base64_encoding import hash_str


def assemble_code(function, **kwargs):
    return "(" + function + " " + " ".join(str(kwargs[arg]) for arg in kwargs) + ")" 

if __name__ == "__main__":
    print(assemble_code(
            "free.tellorflex.submit-value", 
            queryId=hash_str(base64_url_encode("{SpotPrice: [kda,usd]}")), # in pact (hash)
            value=base64_url_encode(str(961782000000000000)), # in pact (base64-encode)
            nonce=0, 
            queryData=base64_url_encode("{SpotPrice: [kda,usd]}"), # in pact (base64-encode)
            staker="reporter1")
        )
    