#!/bin/bash

function prompt_yes_no {
  local answer
  local module="$1"

  echo "Was $module deployed? (yes/no)"
  read answer

  if [ "$answer" == "yes" ]; then
    return 0
  elif [ "$answer" == "no" ]; then
    echo "Should be completed before continuing."
    echo "Exiting."
    return 1
  else
    echo "Invalid answer. Please enter 'yes' or 'no'."
    return 1
  fi
}


if [ "$1" == "help" ]; then
    echo "Usage: $0 [token|tellorflex|storage|autopay|igov|governance]"
    echo "token: deploy the token contract"
    echo "igov: deploy the igov contract"
    echo "tellorflex: deploy the tellorflex contract"
    echo "storage: deploy the queryData storage contract"
    echo "autopay: deploy the autopay contract"
    echo "governance: deploy the governance contract"
    echo "help: show this help message"
    exit 0
    
# signature= | pact add-sig ../keys/admin-1.yaml | pact add-sig ../keys/admin-2.yaml | pact add-sig ../keys/admin-3.yaml
# api= | curl -H "Content-Type: application/json" -d @- https://api.testnet.chainweb.com/chainweb/0.0/testnet04/chain/1/pact/api/v1/send
elif [ "$1" == "request" ]; then
    if [ -z "$2" ]; then
        echo "Usage: $0 request <request-key>"
        exit 1
    fi
    curl -H "Content-Type: application/json" -d "{\"requestKeys\":[\"$2\"]}" -X POST https://api.testnet.chainweb.com/chainweb/0.0/testnet04/chain/1/pact/api/v1/poll

elif [ "$1" == "namespace" ]; then
    echo "Creating namespace..."
    echo "Note: A keyset is allowed one namespace."
    pact --unsigned ./namespace.yaml | pact add-sig ../keys/admin-1.yaml | pact add-sig ../keys/admin-2.yaml | pact add-sig ../keys/admin-3.yaml | curl -H "Content-Type: application/json" -d @- https://api.testnet.chainweb.com/chainweb/0.0/testnet04/chain/1/pact/api/v1/send

elif [ "$1" == "token" ]; then
    if prompt_yes_no "namespace"; then
    echo "Continuing..."
    pact --unsigned ./token-deploy.yaml | pact add-sig ../keys/admin-1.yaml | pact add-sig ../keys/admin-2.yaml | pact add-sig ../keys/admin-3.yaml | curl -H "Content-Type: application/json" -d @- https://api.testnet.chainweb.com/chainweb/0.0/testnet04/chain/1/pact/api/v1/send
    else
    exit 1
    fi

elif [ "$1" == "igov" ]; then
    if prompt_yes_no "token"; then
    echo "Continuing..."
    pact --unsigned ./igov-deploy.yaml | pact add-sig ../keys/admin-1.yaml | pact add-sig ../keys/admin-2.yaml | pact add-sig ../keys/admin-3.yaml | curl -H "Content-Type: application/json" -d @- https://api.testnet.chainweb.com/chainweb/0.0/testnet04/chain/1/pact/api/v1/send
    else
    exit 1
    fi

elif [ "$1" == "tellorflex" ]; then
    if prompt_yes_no "i-governance"; then
    echo "Continuing..."
    pact --unsigned ./tellorflex-deploy.yaml | pact add-sig ../keys/admin-1.yaml | pact add-sig ../keys/admin-2.yaml | pact add-sig ../keys/admin-3.yaml | curl -H "Content-Type: application/json" -d @- https://api.testnet.chainweb.com/chainweb/0.0/testnet04/chain/1/pact/api/v1/send
    else
    exit 1
    fi

elif [ "$1" == "storage" ]; then
    if prompt_yes_no "tellorflex"; then
    echo "Continuing..."
    pact --unsigned ./queryStorage-deploy.yaml | pact add-sig ../keys/admin-1.yaml | pact add-sig ../keys/admin-2.yaml | pact add-sig ../keys/admin-3.yaml | curl -H "Content-Type: application/json" -d @- https://api.testnet.chainweb.com/chainweb/0.0/testnet04/chain/1/pact/api/v1/send
    else
    exit 1
    fi

elif [ "$1" == "autopay" ]; then
    if prompt_yes_no "queryDataStorage"; then
    echo "Continuing..."
    pact --unsigned ./autopay-deploy.yaml | pact add-sig ../keys/admin-1.yaml | pact add-sig ../keys/admin-2.yaml | pact add-sig ../keys/admin-3.yaml | curl -H "Content-Type: application/json" -d @- https://api.testnet.chainweb.com/chainweb/0.0/testnet04/chain/1/pact/api/v1/send
    else
    exit 1
    fi

elif [ "$1" == "governance" ]; then
    if prompt_yes_no "autopay"; then
    echo "Continuing..."
    echo "Governance guard will also be registered."
    pact --unsigned ./governance-deploy.yaml | pact add-sig ../keys/admin-1.yaml | pact add-sig ../keys/admin-2.yaml | pact add-sig ../keys/admin-3.yaml | curl -H "Content-Type: application/json" -d @- https://api.testnet.chainweb.com/chainweb/0.0/testnet04/chain/1/pact/api/v1/send
    else
    exit 1
    fi

else
  echo "Invalid argument. Usage: $0 [namespace|token|igov|tellorflex|storage|autopay|governance|init]"
fi
