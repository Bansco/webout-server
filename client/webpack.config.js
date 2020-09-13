const path = require("path");

module.exports = {
  mode: process.env.NODE_ENV === 'production' ? 'production' : 'development',
  entry: "./src/ws.js",
  output: {
    filename: "main.js",
    path: path.resolve(__dirname, "public"),
  },
};
