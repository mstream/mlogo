if(!self.define){let e,i={};const s=(s,n)=>(s=new URL(s+".js",n).href,i[s]||new Promise((i=>{if("document"in self){const e=document.createElement("script");e.src=s,e.onload=i,document.head.appendChild(e)}else e=s,importScripts(s),i()})).then((()=>{let e=i[s];if(!e)throw new Error(`Module ${s} didn’t register its module`);return e})));self.define=(n,r)=>{const t=e||("document"in self?document.currentScript.src:"")||location.href;if(i[t])return;let o={};const d=e=>s(e,t),c={module:{uri:t},exports:o,require:d};i[t]=Promise.all(n.map((e=>c[e]||d(e)))).then((e=>(r(...e),o)))}}define(["./workbox-3625d7b0"],(function(e){"use strict";self.skipWaiting(),e.clientsClaim(),e.precacheAndRoute([{url:"assets/home-33e6f033.js",revision:null},{url:"assets/index-a0d4cb56.css",revision:null},{url:"index.html",revision:"8e7772496c93299a66175980478880c8"},{url:"registerSW.js",revision:"c5ab56568fb924aeff46d5d96af6ac4a"},{url:"sandbox.html",revision:"1dc27d003505194b6b67d84dbf842bc5"},{url:"pwa-192x192.png",revision:"9eb5a4c27af31c3d671397fea6efbb2a"},{url:"pwa-512x512.png",revision:"fbde72840da99588da6917ed7bc4d11f"},{url:"manifest.webmanifest",revision:"37cf5cf263661c87b2755c6104010fd8"}],{}),e.cleanupOutdatedCaches(),e.registerRoute(new e.NavigationRoute(e.createHandlerBoundToURL("index.html")))}));
