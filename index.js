const { exec } = require("child_process");
const fs = require('fs')

const OUT_FILE = 'out.js'

const PATH = `./${process.argv[2]}`

const INPUT = `${PATH}/${process.argv[3]}`

const registerPorts = main => {
    if (main?.ports?.log) {
        main.ports.log.subscribe(message => console.log(message))
    }
}

const readInput = path => {
    return fs.readFileSync(path, {encoding:'utf8', flag:'r'})
}

exec(`cd ${PATH} && ../node_modules/elm/bin/elm make Main.elm --output ${OUT_FILE}`, (error, stdout, stderr) => {
    if (error) { throw stderr }

    const { Elm } = require(`${PATH}/${OUT_FILE}`)
    const app = Elm.Main.init({
        flags: {
            input: readInput(INPUT)
        } 
    });
    registerPorts(app)
})
