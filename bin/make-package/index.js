const fs = require('fs-extra');
const changeCase = require('change-case')
const { exec } = require('child_process');

const args = process.argv.slice(2)

if (args.length > 0) {
  make()
} else {
  log("You must provide the name of the package. Example:\n\n\tyarn make use-previous")
}

function log(message) {
  console.log(`\n${message}\n`)
}

async function replaceTokens(dir, pkgName, rootName, files) {
  let rePkg = new RegExp("PKG_NAME", "g");
  let reRoot = new RegExp("ROOT_NAME", "g");

  await Promise.all(files.map(async (fileName) => {
    let file = (await fs.readFile(`${dir}/${fileName}`, "utf8"))
      .replace(rePkg, pkgName)
      .replace(reRoot, rootName)

    await fs.writeFile(`${dir}/${fileName}`, file)
  }))
}

async function make() {
  let pkgName = args[0]
  let rootName = changeCase.capitalCase(pkgName).replace(/\s/g, "")
  let newDir = `${__dirname}/../../packages/rescript-${pkgName}`

  try {
    await fs.readdir(newDir)
    log("This package already exits. Please select a different name.")
  } catch (_) {

    await fs.copy(`${__dirname}/package-template`, newDir)

    let files = await fs.readdir(newDir)

    await replaceTokens(newDir, pkgName, rootName, files)

    await fs.writeFile(`${newDir}/${rootName}.res`, "")

    process.chdir(newDir);

    log("Installing dependencies...")

    exec("yarn", (err, stdout, stderr) => {
      if (err) {
        console.error(err)
      } else {
        console.log(stdout);
        console.log(stderr);
      }
    });
  }
}
