const path = require('path');
module.exports = {
  mode: 'development',
  devtool: 'inline-source-map',
  entry: './elm/src/App.ts',
  output: {
    path: __dirname,
    filename: path.join('dist', 'renderer.js'),
  },
  resolve: { extensions: ['.ts', '.js'] },
  devServer: {
    contentBase: __dirname,
    port: 8888,
    historyApiFallback: {
      rewrites: [
        { from: /^\/k/, to: '/' },
      ],
    },
  },
  module: {
    rules: [
      {
        test: /\.ts$/,
        loader: 'awesome-typescript-loader',
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: 'elm-webpack-loader',
          options: {}
        }
      }
    ],
  },
};
