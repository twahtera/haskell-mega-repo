# servant-status

> Servant support for yaml

[![Build Status](https://travis-ci.org/futurice/haskell-servant-status.svg?branch=master)](https://travis-ci.org/futurice/haskell-servant-status)
[![Hackage](https://img.shields.io/hackage/v/servant-status.svg)](http://hackage.haskell.org/package/servant-status)
[![Stackage LTS 2](http://stackage.org/package/servant-status/badge/lts-2)](http://stackage.org/lts-2/package/servant-status)
[![Stackage LTS 3](http://stackage.org/package/servant-status/badge/lts-3)](http://stackage.org/lts-3/package/servant-status)
[![Stackage Nightly](http://stackage.org/package/servant-status/badge/nightly)](http://stackage.org/nightly/package/servant-status)

## Example

Check [`example/Main.hs`](https://github.com/futurice/haskell-servant-status/blob/master/example/Main.hs) for an example:

```
% curl -s localhost:8000/                                 
Example

% curl -s localhost:8000/status                           
status: OK
gc.status: OK
gc.bytesAllocated: 1150648
gc.bytesCopied: 152936
gc.cpu.status: OK
gc.cpu.gcSeconds: 9.26e-4
gc.cpu.mutatorSeconds: 5.533e-3
gc.cpu.seconds: 7.559e-3
gc.cumulativeBytesUsed: 99616
gc.currentBytesSlop: 0
gc.currentBytesUsed: 99616
gc.maxBytesSlop: 15072
gc.maxBytesUsed: 99616
gc.numByteUsageSamples: 1
gc.numGcs: 6
gc.par.status: OK
gc.par.maxBytesCopied: 0
gc.par.totBytesCopied: 0
gc.peakMegabytesAllocated: 1
gc.wall.status: OK
gc.wall.gcSeconds: 2.678278e-3
gc.wall.mutatorSeconds: 57.457006687
gc.wall.seconds: 57.459684965

% curl -s localhost:8000/status.json | python -m json.tool
{
    "gc": {
        "bytesAllocated": 1343256,
        "bytesCopied": 156936,
        "cpu": {
            "gcSeconds": 0.000957,
            "mutatorSeconds": 0.006136,
            "seconds": 0.008193,
            "status": "OK"
        },
        "cumulativeBytesUsed": 99616,
        "currentBytesSlop": 0,
        "currentBytesUsed": 99616,
        "maxBytesSlop": 15200,
        "maxBytesUsed": 99616,
        "numByteUsageSamples": 1,
        "numGcs": 7,
        "par": {
            "maxBytesCopied": 0,
            "status": "OK",
            "totBytesCopied": 0
        },
        "peakMegabytesAllocated": 1,
        "status": "OK",
        "wall": {
            "gcSeconds": 0.002710713,
            "mutatorSeconds": 64.867522132,
            "seconds": 64.870232845,
            "status": "OK"
        }
    },
    "status": "OK"
}
```
