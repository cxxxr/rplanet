const path = require('path');
const fs = require('fs');

module.exports = {
  mode: 'none',
  entry: './.valtan-cache/rplanet.js',
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, '../public/assets/dist')
  },
  resolve: {
    modules: [
      'node_modules',
      fs.readFileSync('.valtan-path', 'utf-8')
    ]
  },
  devtool: 'inline-source-map',
  module: {
    rules: [
      {
        test: /\.js$/,
        use: ["source-map-loader"],
        enforce: "pre"
      }
    ]
  }
};
