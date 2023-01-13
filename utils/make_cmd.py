import json
import time
import utils.base64_encoding as b64
from utils.assemble_pactcode import assemble_code
from datetime import datetime
from nacl.signing import SigningKey, VerifyKey
from nacl.exceptions import BadSignatureError

def verify_signature(msg: str, sig: str, public_key: str) -> bool:
    hashed_msg = b64.hash_bin(msg)
    sig = bytes.fromhex(sig)
    public_key = bytes.fromhex(public_key)
    try:
        VerifyKey(public_key).verify(hashed_msg, sig)
        return True
    except BadSignatureError:
        return False

def sign_msg(msg, key_pair):
    if "public_key" not in key_pair or "secret_key" not in key_pair:
        raise TypeError("Invalid KeyPair: expected to find keys of name 'secretKey' and 'publicKey': " + str(key_pair))
    hsh_bin = b64.hash_bin(msg)
    hsh = b64.b64url_encode_arr(hsh_bin)
    secret_key = SigningKey(seed=bytes.fromhex(key_pair["secret_key"]))
    sig_bin = secret_key.sign(hsh_bin).signature
    return {"hash": hsh, "sig": sig_bin.hex(), "pub_key": key_pair["public_key"]}

def attach_sig(msg, kp_array):
    hsh_bin = b64.hash_bin(msg)
    hsh = b64.b64url_encode_arr(hsh_bin)
    if len(kp_array) == 0:
        return [{"hash": hsh, "sig": None}]
    else:
        return list(
            map(lambda kp: sign_msg(msg, kp) 
            if kp.get("public_key") and kp.get("secret_key") 
            else {
                "hash": hsh,
                "sig": None,
                "public_key": kp["public_key"]
        }, kp_array))

def enforce_type(val, typ, msg):
    assert isinstance(val, typ), "Expected " + msg + " to be of type " + str(typ) + ", but got " + str(type(val)) + " instead"

def mk_meta(sender, chain_id, gas_price, gas_limit, creation_time=int(time.time()), ttl=600):
    enforce_type(sender, str, "sender")
    enforce_type(chain_id, str, "chain_id")
    enforce_type(gas_price, (float, int), "gas_price")
    enforce_type(gas_limit, int, "gas_limit")
    enforce_type(creation_time, int, "creation_time")
    enforce_type(ttl, int, "ttl")
    return {
        "creationTime": creation_time,
        "ttl": ttl,
        "gasLimit": gas_limit,
        "chainId": chain_id,
        "gasPrice": gas_price,
        "sender": sender
    }

def as_array(single_or_array):
    if isinstance(single_or_array, list):
        return single_or_array
    else:
        return [single_or_array]

def pull_sig(s):
    if "sig" not in s:
        raise TypeError("Expected to find keys of name 'sig' in " + str(s))
    return {"sig": s["sig"]}

def pull_and_check_hashes(sigs):
    hsh = sigs[0]["hash"]
    for i in range(1, len(sigs)):
        if sigs[i]["hash"] != hsh:
            raise TypeError("Sigs for different hashes found: " + str(sigs))
    return hsh

def enforce_array(val, msg):
    if not isinstance(val, list):
        raise TypeError(msg + " must be an array: " + str(val))

def mk_single_cmd(sigs, cmd):
    enforce_array(sigs, "sigs")
    enforce_type(cmd, str, "cmd")
    return {
        "hash": pull_and_check_hashes(sigs),
        "sigs": list(map(pull_sig, filter(lambda sig: sig["sig"], sigs))),
        "cmd": cmd
    }

def mk_signer(kp):
    if "clist" in kp:
        return {"clist": as_array(kp["clist"]), "pubKey": kp["public_key"]}
    else:
        return {"pubKey": kp["public_key"]}


def prepare_exec_cmd(pact_code, key_pairs, nonce,  env_data, meta, network_id):
    enforce_type(nonce, str, "nonce")
    enforce_type(pact_code, str, "pactCode")
    kp_array = as_array(key_pairs)
    signers = list(map(mk_signer, kp_array))
    cmd_json = {
        "networkId": network_id,
        "payload":{
            "exec":{
                "data": env_data or None,
                "code": pact_code
            }
        },
        "signers": signers,
        "meta": meta,
        "nonce": nonce
    }
    cmd = json.dumps(cmd_json, separators=(',', ':'))
    sigs = attach_sig(cmd, kp_array)
    return mk_single_cmd(sigs, cmd)

def mk_public_send(cmds):
    return json.dumps({"cmds": as_array(cmds)}, separators=(',', ':'))

def formatted_time():
    return datetime.utcnow().isoformat() + " UTC"

def simple_exec_cmd(
    pact_code,
    meta, 
    key_pairs=[],
    nonce=formatted_time(),
    env_data={},
    network_id=None):
    return mk_public_send(
        prepare_exec_cmd(
            key_pairs=key_pairs, 
            nonce=nonce, 
            pact_code=pact_code, 
            env_data=env_data, 
            meta=meta, 
            network_id=network_id))

if __name__ == "__main__":
    print(simple_exec_cmd(
        key_pairs=[{
            "public_key":"12465ff1907370c2b67c2601c6914a5ec921b65c3dba876b89566ee5420c530e",
            "secret_key":"56c99fc2c74486ea5ead9ae8bbc7efdb4d30554d433b9abce28ed1bc2e0b5131"}],
        pact_code=assemble_code(
                    "free.tellorflex.submit-value", 
                    queryId=b64.hash_str(b64.base64_url_encode("{SpotPrice: [kda,usd]}")),
                    value=b64.base64_url_encode(str(961782000000000000)),
                    nonce=0, 
                    queryData=b64.base64_url_encode("{SpotPrice: [kda,usd]}"),
                    staker="reporter1"),
        meta=mk_meta(
                sender="reporter1", 
                chain_id="1",
                gas_limit=150000,
                ttl=600,
                gas_price=1e-7),
        network_id="testnet04"))