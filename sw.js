if(!self.define){let e,s={};const i=(i,n)=>(i=new URL(i+".js",n).href,s[i]||new Promise((s=>{if("document"in self){const e=document.createElement("script");e.src=i,e.onload=s,document.head.appendChild(e)}else e=i,importScripts(i),s()})).then((()=>{let e=s[i];if(!e)throw new Error(`Module ${i} didn’t register its module`);return e})));self.define=(n,r)=>{const o=e||("document"in self?document.currentScript.src:"")||location.href;if(s[o])return;let t={};const l=e=>i(e,o),a={module:{uri:o},exports:t,require:l};s[o]=Promise.all(n.map((e=>a[e]||l(e)))).then((e=>(r(...e),t)))}}define(["./workbox-9517df1b"],(function(e){"use strict";self.skipWaiting(),e.clientsClaim(),e.precacheAndRoute([{url:"assets/home-2427bc17.js",revision:null},{url:"assets/sandbox-2427bc17.js",revision:null},{url:"assets/webapp-c202383a.css",revision:null},{url:"assets/webapp-fa69a589.js",revision:null},{url:"index.html",revision:"4ebd46adacda3cb9240bd85a6ff9e460"},{url:"registerSW.js",revision:"c5ab56568fb924aeff46d5d96af6ac4a"},{url:"sandbox/index.html",revision:"16220e3fc7e94ce27f0b9f73b4bfbf49"},{url:"pwa-192x192.png",revision:"9eb5a4c27af31c3d671397fea6efbb2a"},{url:"pwa-512x512.png",revision:"fbde72840da99588da6917ed7bc4d11f"},{url:"bucket.png",revision:"4493a90062a862e0d60353bb143e226b"},{url:"manifest.webmanifest",revision:"7796ee7d4947f4b7e6d969f06515e34b"}],{}),e.cleanupOutdatedCaches(),e.registerRoute(new e.NavigationRoute(e.createHandlerBoundToURL("index.html")))}));
