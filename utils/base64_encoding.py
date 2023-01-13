import base64
import hashlib
from nacl import encoding
from nacl import signing
from nacl import bindings

def base64_url_encode(input_str):
    return base64.urlsafe_b64encode(input_str.encode()).rstrip(b'=').decode()

def base64_url_decode(input_str):
    input_str = input_str.encode()
    rem = len(input_str) % 4
    if rem > 0:
        input_str += b'=' * (4 - rem)
    return base64.urlsafe_b64decode(input_str).decode()

def b64url_encode_arr(input: bytes) -> str:
    return base64.urlsafe_b64encode(input).rstrip(b'=').decode()

def b64url_decode_arr(input: str) -> bytes:
    return base64.urlsafe_b64decode(input)

def hash_bin(s: str) -> bytes:
    h = hashlib.blake2b(digest_size=32)
    h.update(s.encode())
    return h.digest()

def hash_str(s: str) -> str:
    return b64url_encode_arr(hash_bin(s))

def gen_key_pair():
    kp = bindings.crypto_sign_keypair()
    pub_key = kp[0].hex()
    sec_key = kp[1].hex()[:64]
    return {"public_key": pub_key, "secret_key": sec_key}

def restoreKeyPairFromsecret_key(seed: str) -> dict:
    if not seed:
        raise ValueError("seed for KeyPair generation not provided")
    if len(seed) != 64:
        raise ValueError("Seed for KeyPair generation has bad size")
    seed_for_nacl = bytes.fromhex(seed)
    kp = signing.SigningKey(seed_for_nacl)
    pub_key = kp.verify_key.encode(encoding.HexEncoder).decode()
    sec_key = kp.encode(encoding.HexEncoder).decode()[:64]
    return { "public_key": pub_key, "secret_key": sec_key }

if __name__ == '__main__':
    print(base64_url_encode("100"))
    print(base64_url_decode("MTAw"))
    assert base64_url_encode("100") == "MTAw"
    assert base64_url_decode("MTAw") == "100"
    print(b64url_encode_arr(hash_bin("e1Nwb3RQcmljZTogW2V0aCx1c2RdfQ")))
    print(f"Generate a keyPair  {gen_key_pair()}")
    print(f"Restore a keyPair  {restoreKeyPairFromsecret_key('f92089d02de9f01df0bf53c9d9d677dee960826640bc39f1c45234cc13d66683')}")