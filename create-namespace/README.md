### Create a namespace (for testnet)

#### Pre-Req
* Pact install

1. Replace public keys in [namespace.yaml](./namespace.yaml) and for gas payer add coin account name!
2. Replace keys in [gas-account.yaml](./gas-account.yaml)
3. Run this command
```console
pact --unsigned ./namespace.yaml | pact add-sig ./gas-account.yaml | curl -H "Content-Type: application/json" -d @- https://api.testnet.chainweb.com/chainweb/0.0/testnet04/chain/1/pact/api/v1/send
```
1. After step three a "requestKeys" hash will print and you can access the transaction and grab the namespace from the return object using either the following
```console
curl -H "Content-Type: application/json" -d '{"requestKeys":["paste request key here"]}' -X POST https://api.testnet.chainweb.com/chainweb/0.0/testnet04/chain/1/pact/api/v1/poll
```
or going to the [kadena-explorer](https://explorer.chainweb.com/testnet) and searching the request key.

Feel free to use gas-account to test this out