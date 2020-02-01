const path = require('path');
const fs = require('fs');

module.exports = {
  mode: 'none',
  entry: './rplanet.js',
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, '../public/assets/dist')
  },
  resolve: {
    modules: [
      'node_modules',
      fs.readFileSync('.valtan-path', 'utf-8')
    ]
  }
};
