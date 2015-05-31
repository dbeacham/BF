curl \
    "https://api.betfair.com/exchange/betting/json-rpc/v1" \
    -H "X-Authentication: $BF_SESSION_TOKEN" \
    -H "X-Application: $BF_APPLICATION" \
    -d @$1

