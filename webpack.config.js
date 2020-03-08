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
        loaders: [
          {
            loader: 'elm-webpack-loader',
            options: {
              cwd: __dirname,
              debug: true,
            },
          },
        ],
      },
    ],
  },
};
