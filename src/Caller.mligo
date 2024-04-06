#import "@ligo/fa/lib/main.mligo" "FA2"
module C = struct
    type register = (address,nat)big_map
    type storage = {
        ledger: register;
        admin: address;
        token_address: address;
        start_date : timestamp;
        end_date : timestamp;
        freezing_duration : int; // in days
    }

    module Errors = struct
        let too_early = "Contract still not open"
        let too_late = "Contract already closed"
        let ins_balance = "Balance too low"
    end

type result = operation list * storage

let updateValue(m,key,value:register*address*nat):register = Big_map.update key (Some value) m

let get_entrypoint(addr, name:address*string) = 
    if name = "transfer" then
        match Tezos.get_entrypoint_opt "%transfer" addr with
            | Some contract -> contract
            | None -> failwith "transfer not found"
    else
        failwith "Unsupported entrypoint"

[@entry] let deposit (amount_:nat) (s : storage) : result =
    let _ = assert_with_error (Tezos.get_now() >= s.start_date) Errors.too_early in
    let _ = assert_with_error (Tezos.get_now() <= s.end_date) Errors.too_late in
    let transfer_requests = ([
        ({from_=Tezos.get_sender(); txs=([{to_=Tezos.get_self_address();token_id=0n;amount=amount_}] : FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
    ] : FA2.SingleAssetExtendable.TZIP12.transfer) in
    let transfer : FA2.SingleAssetExtendable.TZIP12.transfer contract = get_entrypoint(s.token_address, "transfer") in
    let op = Tezos.transaction transfer_requests 0mutez transfer in
    let old_balance = match Big_map.find_opt (Tezos.get_sender()) s.ledger with
        |Some l -> l
        |None -> 0n
    in
    [op], {s with ledger = updateValue(s.ledger, (Tezos.get_sender()), amount_ + old_balance)}

[@entry] let claim (amount_:nat)(s: storage):result=
    let _ = assert_with_error (Tezos.get_now() >= (s.start_date + s.freezing_duration)) Errors.too_early in
    let old_balance = match Big_map.find_opt (Tezos.get_sender()) s.ledger with
        |Some l -> l
        |None -> 0n
    in
    let _ = assert_with_error (amount_ <= old_balance) Errors.ins_balance in
    let transfer_requests = ([
        ({from_=Tezos.get_self_address(); txs=([{to_=Tezos.get_sender();token_id=0n;amount=amount_}] : FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
    ] : FA2.SingleAssetExtendable.TZIP12.transfer) in
    let transfer : FA2.SingleAssetExtendable.TZIP12.transfer contract = get_entrypoint(s.token_address, "transfer") in
    let op = Tezos.transaction transfer_requests 0mutez transfer in
    [op], {s with ledger = updateValue(s.ledger, (Tezos.get_sender()), abs(old_balance - amount_))}

    
end