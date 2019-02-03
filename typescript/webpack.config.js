const path = require('path');

module.exports = {
  entry: './application/application.js',
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, 'application')
  }
};