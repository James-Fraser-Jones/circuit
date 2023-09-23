const express = require('express');
const bodyParser = require('body-parser');
const { exec } = require('child_process');

const app = express();
const port = 3000;

app.use(bodyParser.text());

app.post('/reduce', (req, res) => {
  let expr = req.body;
  //console.log(`Recieved Expr: ${expr}`)
  let command = `../.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.2.1.0/build/circuit-exe/circuit-exe -e "${expr}"`
  exec(command, (error, stdout, stderr) => {
    if (error) {
      return res.status(500).send('Error executing the command.');
    }
    res.set('Content-Type', 'text/plain');
    res.setHeader('Access-Control-Allow-Origin', '*'); // Allow requests from any origin
    res.setHeader('Access-Control-Allow-Methods', 'POST');
    res.setHeader('Access-Control-Allow-Headers', 'Content-Type');
    res.status(200).send(`${stdout}`);
    //console.log(`Returned Output:\n${stdout}`)
  });
});

app.listen(port, () => {
  console.log(`Server is running on port ${port}`);
});