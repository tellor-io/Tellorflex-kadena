Deployment checklist
#### Keysets
* Create admin keyset
* make an account in coin module and Fund keyset (for gas)
* create namespace

#### Check if accounts exist in token module cause these need to be created from within the module
* tellorflex account
* governance account
* autopay account

#### Deploy order
1. token(testnet)
2. i-governance
3. tellorflex
4. queryDataStorage
5. autopay
6. governance (tellorflex is initialized in this step by default)
