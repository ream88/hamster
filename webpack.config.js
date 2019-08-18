const path = require('path')
const CopyWebpackPlugin = require('copy-webpack-plugin')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const UglifyJsPlugin = require('uglifyjs-webpack-plugin')

module.exports = {
  mode: process.env.NODE_ENV || 'development',
  entry: path.join(__dirname, 'src', 'index.js'),
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'hamster.js'
  },
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: 'elm-webpack-loader',
          options: {
            debug: process.env.NODE_ENV !== 'production',
            optimize: process.env.NODE_ENV === 'production'
          }
        }
      }
    ]
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: path.join(__dirname, 'src', 'index.html'),
      hash: true
    }),
    new CopyWebpackPlugin([
      { from: path.join(__dirname, 'assets', '*'), flatten: true }
    ])
  ],
  optimization: {
    minimizer: [
      new UglifyJsPlugin({
        test: /src\/index\.js$/,
        uglifyOptions: {
          compress: {
            pure_funcs: ['F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9'],
            pure_getters: true,
            keep_fargs: false,
            unsafe_comps: true,
            unsafe: true,
            passes: 3
          }
        }
      })
    ]
  },
  devServer: {
    contentBase: path.join(__dirname, 'assets'),
    port: 8080
  }
}
