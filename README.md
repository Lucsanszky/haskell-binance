# haskell-binance

An unofficial Haskell wrapper for the Binance cryptocurrency exchange API. 
Work in progress. 

## Rest API Status

- [ ] `GET /api/v1/ping`
- [x] `GET /api/v1/time`
- [ ] `GET /api/v1/exchangeInfo`
- [ ] `GET /api/v1/depth`
- [ ] `GET /api/v1/trades`
- [ ] `GET /api/v1/historicalTrades`
- [ ] `GET /api/v1/aggTrades`
- [ ] `GET /api/v1/klines`
- [ ] `GET /api/v1/ticker/24hr`
- [ ] `GET /api/v3/ticker/price`
- [ ] `GET /api/v3/ticker/bookTicker`
- [ ] `POST /api/v3/order  (HMAC SHA256)`
- [x] `POST /api/v3/order/test (HMAC SHA256)`
- [ ] `GET /api/v3/order (HMAC SHA256)`
- [ ] `DELETE /api/v3/order (HMAC SHA256)`
- [ ] `GET /api/v3/openOrders (HMAC SHA256)`
- [x] `GET /api/v3/allOrders (HMAC SHA256)`
- [ ] `GET /api/v3/account (HMAC SHA256)`
- [ ] `GET /api/v3/myTrades (HMAC SHA256)`

## User Data Streams Status

- [ ] `POST /api/v1/userDataStream`
- [ ] `PUT /api/v1/userDataStream`
- [ ] `DELETE /api/v1/userDataStream`

## WebSocket Streams Status

- [ ] &lt;symbol>@aggTrade
- [ ] &lt;symbol>@trade
- [ ] &lt;symbol>@kline_&lt;interval>
- [ ] &lt;symbol>@ticker
- [ ] !ticker@arr
- [ ] &lt;symbol>@depth&lt;levels>
- [x] &lt;symbol>@depth

## Withdrawal API Status

- [ ] `POST /wapi/v3/withdraw.html (HMAC SHA256)`
- [ ] `GET /wapi/v3/depositHistory.html (HMAC SHA256)`
- [ ] `GET /wapi/v3/withdrawHistory.html (HMAC SHA256)`
- [ ] `GET  /wapi/v3/depositAddress.html (HMAC SHA256)`
- [ ] `GET  /wapi/v3/withdrawFee.html (HMAC SHA256)`
- [ ] `GET /wapi/v3/accountStatus.html`
- [ ] `GET /wapi/v3/systemStatus.html`
