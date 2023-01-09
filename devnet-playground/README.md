# devnet-playground

<details>
  <summary> Ubuntu 22.04 </summary>


  - [Docker install](#docker-install)
  - [Install Nix](#install-nix)
  - [Devnet deployment](#devnet-deployment)

<a id="Docker install"></a>
## [Docker](https://docs.docker.com/engine/install/ubuntu/) install
1. Uninstall old versions (skip if docker never installed)
```console
sudo apt-get remove docker docker-engine docker.io containerd runc
```
1a.  if fails try this:
```console
sudo apt-get purge docker-ce docker-ce-cli containerd.io docker-compose-plugin
```
```console
sudo rm -rf /var/lib/docker
sudo rm -rf /var/lib/containerd
```
2. Install using repository
```console
sudo apt-get update
sudo apt-get install \
    ca-certificates \
    curl \
    gnupg \
    lsb-release
```

3. Add Docker’s official GPG key:
```console 
sudo mkdir -p /etc/apt/keyrings
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
```
4. Use the following command to set up the repository:
```console
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
```
5. Update the `apt` package index:
```console
sudo apt-get update
```
6. Install Docker Engine, containerd, and Docker Compose.
```console
sudo apt-get install docker-ce docker-ce-cli containerd.io docker-compose-plugin
```
7. Confirm docker compose install
```console
docker compose version
```

<a id="nix-install"></a>
## Install [Nix](https://nixos.org/download.html)

1. Multi-user installation:
```console
sh <(curl -L https://nixos.org/nix/install) --daemon
```

<a id="devnet-deploy"></a>
## Devnet deployment
1. Clone repo
```console
git clone https://github.com/tellor-io/Tellorflex-kadena.git
cd Tellorflex-kadena/devnet-playground
find scripts -type f -exec chmod +x {} +
```
2. Enter to nix environment
```console
nix-shell
```
3. Deployments
```
# start devnet
devnet-start

# deploy iflex and igovernance
deploy-interfaces

# deploy governance contract
governance-deploy

# deploy tellorflex contract
tellorflex-deploy

# initialize both tellorflex and governance
init-module

# reset devnet and start over
devnet-clean && devnet-start
```
</details>

<details>
  <summary> MacOS </summary>
</details>
:fire: Used this awesome repo/tutorial as a template
https://github.com/thomashoneyman/real-world-pact
