import Arweave from 'arweave/node';
import fs from 'mz/fs';
import crypto from 'mz/crypto';

const arweave = Arweave.init({
  host: 'arweave.net',
  port: 443,
  protocol: 'https'
})

/*
const arweave = Arweave.init({
  host: '127.0.0.1',
  port: 1984
})
*/

const keyfile = 'key.json'

const generate = () => {
  // const size: number = 2 ** 30
  // 1 GB
  // const size: number = 2 ** 20
  // 1 MB
  const size: number = 1 * (2 ** 20)
  // 1 MB
  console.log('Generating random buffer of length', size, 'bytes')
  const buffer = Buffer.alloc(size)
  crypto.randomFillSync(buffer)
  return buffer
}

const sleep = (time: number) => {
  return new Promise((resolve) => setTimeout(resolve, time));
}

try {
  (async () => {
    const exists: boolean = await fs.exists(keyfile) 
    var key: any
    if (exists) {
      console.log('Found existing keyfile')
      key = JSON.parse(fs.readFileSync(keyfile).toString('utf-8'))
    } else {
      console.log('No keyfile found, generating key and writing keyfile')
      key = await arweave.wallets.generate()
      fs.writeFileSync(keyfile, Buffer.from(JSON.stringify(key), 'utf-8'))
    }
    const address: string = await arweave.wallets.jwkToAddress(key)
    console.log('Address:', address)
    var last_hash: string = ''
    for (var i = 0; i = 1000000; i++) {
      const balance: string = await arweave.wallets.getBalance(address)
      const ar: string = arweave.ar.winstonToAr(balance)
      console.log('Balance (AR):', ar)
      var opts: any = {
        data: generate(),
        reward: arweave.ar.arToWinston('0.005')
      }
      /*
      if (last_hash !== '') {
        opts.last_tx = last_hash
      }
      */
      const tx = await arweave.createTransaction(opts, key)
      console.log('Transaction cost: ', arweave.ar.winstonToAr(tx.reward))
      console.log('Signing transaction...')
      await arweave.transactions.sign(tx, key)
      console.log('Last tx:', tx.last_tx)
      console.log('Hash:', tx.id)
      last_hash = tx.id
      const response = await arweave.transactions.post(tx)
      console.log('Response:', response.status, response.data)
      await sleep(30 * 1000)
    }
  })()
} catch(err) {
  console.log(err)
}
