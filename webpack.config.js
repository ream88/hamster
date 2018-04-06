const path = require('path')

module.exports = {
  mode: 'development',
  entry: path.join(__dirname, 'src', 'index.js'),
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'hamster.js'
  },
  module: {
    rules: [
      {
        test: /\.elm/,
        loader: 'elm-webpack-loader'
      }
    ]
  }
}
