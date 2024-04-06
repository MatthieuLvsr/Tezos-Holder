#import "@ligo/fa/lib/main.mligo" "FA2"


type extension = {
    admin : address
}

type operator = address

type operators = (address, operator set) big_map

type storage = extension FA2.SingleAssetExtendable.storage
type ret = operation list * storage