const exec = require('child_process').exec;
require('dotenv').config();


totalToken = "'" + 'X-SHOPIFY-STOREFRONT-ACCESS-TOKEN: ' + process.env.X_SHOPIFY_STOREFRONT_ACCESS_TOKEN + "'"
url = process.env.SHOP_GRAPHQL_URL
cmd = "elm-graphql " + url + " --base ShopifyApi --header " + totalToken + " --header 'Content-Type: application/json'";
exec(cmd);
