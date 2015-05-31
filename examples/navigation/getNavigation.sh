curl \
    -v \
    "https://api.betfair.com/exchange/betting/rest/v1/en/navigation/menu.json" \
    -H "X-Application: $BF_APPLICATION" \
    -H "X-Authentication: $BF_SESSION_TOKEN" \
    | python -m json.tool \
    > navigation.json
