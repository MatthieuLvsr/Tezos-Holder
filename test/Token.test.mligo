#import "../src/Token.mligo" "Token"
#import "./helper/list.mligo" "List_helper"
#import "./helper/assert.mligo" "Assert"

type ext = Token.extension
type storage = Token.storage

let get_initial_storage (a, b, c : nat * nat * nat) =
  let () = Test.reset_state 6n ([] : tez list) in

  let owner1 = Test.nth_bootstrap_account 0 in
  let owner2 = Test.nth_bootstrap_account 1 in
  let owner3 = Test.nth_bootstrap_account 2 in

  let owners = [owner1; owner2; owner3] in

  let op1 = Test.nth_bootstrap_account 3 in
  let op2 = Test.nth_bootstrap_account 4 in
  let op3 = Test.nth_bootstrap_account 5 in

  let ops = [op1; op2; op3] in

  let ledger = Big_map.literal ([
      (owner1, a);
      (owner2, b);
      (owner3, c);
    ])
  in

  let operators  = Big_map.literal ([
      (owner1, Set.literal [op1]);
      (owner2, Set.literal [op1;op2]);
      (owner3, Set.literal [op1;op3]);
      (op3   , Set.literal [op1;op2]);
    ])
  in

  let token_info = (Map.empty: (string, bytes) map) in
  let token_data = {
    token_id   = 0n;
    token_info = token_info;
  } in
  let token_metadata = Big_map.literal ([
    (0n, token_data);
  ])
  in


 let metadata =Big_map.literal [
	("", [%bytes {|tezos-storage:data|}]);
	("data", [%bytes
{|{
	"name":"FA2",
	"description":"Example FA2 implementation",
	"version":"0.1.0",
	"license":{"name":"MIT"},
	"authors":["Benjamin Fuentes<benjamin.fuentes@marigold.dev>"],
	"homepage":"",
	"source":{"tools":["Ligo"], "location":"https://github.com/ligolang/contract-catalogue/tree/main/lib/fa2"},
	"interfaces":["TZIP-012"],
	"errors":[],
	"views":[]

}|}]);
]  in

  let initial_storage: Token.storage = {
      ledger         = ledger;
      metadata       = metadata;
      token_metadata = token_metadata;
      operators      = operators;
      extension      = {
        admin = owner1
      }
  } in

  initial_storage, owners, ops

let assert_balances
  (contract_address : (Token parameter_of, Token.storage) typed_address )
  (a, b, c : (address * nat) * (address * nat) * (address * nat)) =
  let (owner1, balance1) = a in
  let (owner2, balance2) = b in
  let (owner3, balance3) = c in
  let storage = Test.get_storage contract_address in
  let ledger = storage.ledger in
  let () = match (Big_map.find_opt owner1 ledger) with
    Some amt -> assert (amt = balance1)
  | None -> failwith "incorret address"
  in
  let () = match (Big_map.find_opt owner2 ledger) with
    Some amt ->  assert (amt = balance2)
  | None -> failwith "incorret address"
  in
  let () = match (Big_map.find_opt owner3 ledger) with
    Some amt -> assert (amt = balance3)
  | None -> failwith "incorret address"
  in
  ()


let test_atomic_tansfer_success =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let { addr;code = _code; size = _size} = Test.originate (contract_of Token) initial_storage 0tez in
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;token_id=0n;amount=2n};{to_=owner3;token_id=0n;amount=3n}] : Token.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
    ({from_=owner2; txs=([{to_=owner3;token_id=0n;amount=2n};{to_=owner1;token_id=0n;amount=3n}] : Token.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
  ] : Token.FA2.SingleAssetExtendable.TZIP12.transfer)
  in
  let () = Test.set_source op1 in
  let contr = Test.to_contract addr in
  let _ = Test.transfer_to_contract_exn contr (Transfer transfer_requests) 0tez in
  let () = assert_balances addr ((owner1, 8n), (owner2, 7n), (owner3, 15n)) in
  ()

let test_atomic_tansfer_fail_authorisation =
  let initial_storage, owners, operators = get_initial_storage (10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let { addr;code = _code; size = _size} = Test.originate (contract_of Token) initial_storage 0tez in
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;token_id=0n;amount=2n};{to_=owner3;token_id=0n;amount=3n}] : Token.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
    ({from_=owner2; txs=([{to_=owner3;token_id=0n;amount=2n};{to_=owner1;token_id=0n;amount=3n}] : Token.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
  ] : Token.FA2.SingleAssetExtendable.TZIP12.transfer)
  in
  let () = Test.set_source op1 in
  let contr = Test.to_contract addr in
  // owner3 not operator for owner1 & owner2 -> transaction should fail
  let () = Test.set_source(owner3) in 
  let r = Test.transfer_to_contract contr (Transfer transfer_requests) 0tez in
  Assert.string_failure r  Token.FA2.SingleAssetExtendable.Errors.not_operator

let test_atomic_tansfer_fail_ins_balance =
  let initial_storage, owners, operators = get_initial_storage (10n, 0n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let { addr;code = _code; size = _size} = Test.originate (contract_of Token) initial_storage 0tez in
  let transfer_requests = ([
    ({from_=owner1; txs=([{to_=owner2;token_id=0n;amount=2n};{to_=owner3;token_id=0n;amount=3n}] : Token.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
    ({from_=owner2; txs=([{to_=owner3;token_id=0n;amount=2n};{to_=owner1;token_id=0n;amount=3n}] : Token.FA2.SingleAssetExtendable.TZIP12.atomic_trans list)});
  ] : Token.FA2.SingleAssetExtendable.TZIP12.transfer)
  in
  let () = Test.set_source op1 in
  let contr = Test.to_contract addr in
  let r = Test.transfer_to_contract contr (Transfer transfer_requests) 0tez in
  Assert.string_failure r Token.FA2.SingleAssetExtendable.Errors.ins_balance