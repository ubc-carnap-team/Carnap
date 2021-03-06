if (document.TruthTrees && document.TruthTrees.length) {
  const scriptTag = document.createElement("script");
  scriptTag.src = "https://unpkg.com/truth-tree@latest/dist/lib.js";
  scriptTag.onload = (err) => {
    return document.TruthTrees.forEach((args) => Rudolf.createTree(...args));
  };
  document.body.appendChild(scriptTag);
}
