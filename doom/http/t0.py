# -*- Mode: Python -*-

import hpack

data = [
    #"885f87497ca589d34d1f628fe4c7f21742f3e275a6db5d97021fe76496df3dbf4a05b521b665040134a085702ddc65953168df",
    #"885f8b352398ac23bcdfefcd347f6291e4c7f216c4ebef01c75a5c4eb2fb2eff3fc0",
    #"88bf6291e4c7f216c4ebef34fb4f2ed3e07da7bf9fc1",
    #"88c06291e4c7f216c4ebec8590b4bbcebc06dffe7fc2"
    #15
    "885f87497ca589d34d1f628fe4c7f21742f3e275a6db5d97021fe76496c361be940b8a436cca0802694035700f5c6c4a62d1bf",
    #17
    "885f8b352398ac23bcdfefcd347f6291e4c7f216c4ebef01c75a5c4eb2fb2eff3fc0",
    #21
    "88c06291e4c7f216c4ebec8590b4bbcebc06dffe7fc2",
    #25
    "88c20f1391e4c7f216c4ebeebe169e5dd7df6d97bf9fc3",
    #19
    "88bf6291e4c7f216c4ebef34fb4f2ed3e07da7bf9fc1",
    #23
    "88c16291e4c7f216c4ebeeb8271d5dd7d96d97bf9fc3",
    #27
    "88c20f1391e4c7f216c4ebeebe27da5c4e3adb4cff3fc3",
    ]

# re-ordered to the proper order:
data = [
    #15
    "885f87497ca589d34d1f628fe4c7f21742f3e275a6db5d97021fe76496c361be940b8a436cca0802694035700f5c6c4a62d1bf",
    #17
    "885f8b352398ac23bcdfefcd347f6291e4c7f216c4ebef01c75a5c4eb2fb2eff3fc0",
    #19
    "88bf6291e4c7f216c4ebef34fb4f2ed3e07da7bf9fc1",
    #21
    "88c06291e4c7f216c4ebec8590b4bbcebc06dffe7fc2",
    #23
    "88c16291e4c7f216c4ebeeb8271d5dd7d96d97bf9fc3",
    #25
    "88c20f1391e4c7f216c4ebeebe169e5dd7df6d97bf9fc3",
    #27
    "88c20f1391e4c7f216c4ebeebe27da5c4e3adb4cff3fc3",
]

d = hpack.Decoder()

for item in data:
    print (d.decode (bytes.fromhex (item)))
