#import "../src/Caller.mligo" "Caller"
#import "./helper/list.mligo" "List_helper"
#import "../src/Token.mligo" "Token"
#import "./helper/assert.mligo" "Assert"

type ext = Token.extension
type storage = Token.storage

let get_initial_storage (date, a, b, c : timestamp * nat * nat * nat) =
  let () = Test.reset_state_at date 6n ([] : tez list) in

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

let assert_caller_balances
  (contract_address : (Caller.C parameter_of, Caller.C.storage) typed_address )
  (a:address * nat) =
  let (owner1, balance1) = a in
  let storage = Test.get_storage contract_address in
  let ledger = storage.ledger in
  let () = match (Big_map.find_opt owner1 ledger) with
    Some amt -> assert (amt = balance1)
  | None -> failwith "incorret address"
  in
  ()

  let test_deposit_success =
  let initial_date : timestamp = ("2024-02-01t00:00:00Z" : timestamp) in
  let start_date : timestamp = ("2024-01-01t00:00:00Z" : timestamp) in
  let end_date : timestamp = ("2024-03-01t00:00:00Z" : timestamp) in
  let freezing_duration : int = 86_400 * 15 in
  let initial_storage, owners, operators = get_initial_storage (initial_date, 10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let _op1    = List_helper.nth_exn 0 operators in
  let { addr = token_addr;code = _code; size = _size} = Test.originate (contract_of Token) initial_storage 0tez in
  let initial_storage_caller = {ledger = Big_map.literal[]; admin = owner1; token_address = Test.to_address token_addr;start_date=start_date;end_date=end_date;freezing_duration=freezing_duration} in
  let { addr = caller_addr;code = _code; size = _size} = Test.originate (contract_of Caller.C) initial_storage_caller 0tez in
  let contr = Test.to_contract caller_addr in
  let () = Test.set_source owner1 in
  let _ = Test.transfer_exn token_addr
    (Update_operators ([
      (Add_operator ({
        owner    = owner1;
        operator = (Test.to_address caller_addr);
        token_id = 0n;
      } : Token.FA2.SingleAssetExtendable.TZIP12.operator) : Token.FA2.SingleAssetExtendable.TZIP12.unit_update);
    ] : Token.FA2.SingleAssetExtendable.TZIP12.update_operators)) 0tez in
  let _ = Test.transfer_to_contract_exn contr (Deposit 5n ) 0tez in
  let () = assert_caller_balances caller_addr (owner1, 5n) in
  let () = assert_balances token_addr ((owner1, 5n), (owner2, 10n), (owner3, 10n)) in
  ()

  let test_deposit_fail_date_too_early =
  let initial_date : timestamp = ("2024-01-01t00:00:00Z" : timestamp) in
  let start_date : timestamp = ("2024-02-01t00:00:00Z" : timestamp) in
  let end_date : timestamp = ("2024-03-01t00:00:00Z" : timestamp) in
  let freezing_duration : int = 86_400 * 15 in
  let initial_storage, owners, operators = get_initial_storage (initial_date, 10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let _op1    = List_helper.nth_exn 0 operators in
  let { addr = token_addr;code = _code; size = _size} = Test.originate (contract_of Token) initial_storage 0tez in
  let initial_storage_caller = {ledger = Big_map.literal[]; admin = owner1; token_address = Test.to_address token_addr;start_date=start_date;end_date=end_date;freezing_duration=freezing_duration} in
  let { addr = caller_addr;code = _code; size = _size} = Test.originate (contract_of Caller.C) initial_storage_caller 0tez in
  let contr = Test.to_contract caller_addr in
  let () = Test.set_source owner1 in
  let _ = Test.transfer_exn token_addr
    (Update_operators ([
      (Add_operator ({
        owner    = owner1;
        operator = (Test.to_address caller_addr);
        token_id = 0n;
      } : Token.FA2.SingleAssetExtendable.TZIP12.operator) : Token.FA2.SingleAssetExtendable.TZIP12.unit_update);
    ] : Token.FA2.SingleAssetExtendable.TZIP12.update_operators)) 0tez in
  let r = Test.transfer_to_contract contr (Deposit 5n ) 0tez in
  Assert.string_failure r Caller.C.Errors.too_early

  let test_deposit_fail_date_too_late =
  let initial_date : timestamp = ("2024-03-01t00:00:00Z" : timestamp) in
  let start_date : timestamp = ("2024-01-01t00:00:00Z" : timestamp) in
  let end_date : timestamp = ("2024-02-01t00:00:00Z" : timestamp) in
  let freezing_duration : int = 86_400 * 15 in
  let initial_storage, owners, operators = get_initial_storage (initial_date, 10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let _op1    = List_helper.nth_exn 0 operators in
  let { addr = token_addr;code = _code; size = _size} = Test.originate (contract_of Token) initial_storage 0tez in
  let initial_storage_caller = {ledger = Big_map.literal[]; admin = owner1; token_address = Test.to_address token_addr;start_date=start_date;end_date=end_date;freezing_duration=freezing_duration} in
  let { addr = caller_addr;code = _code; size = _size} = Test.originate (contract_of Caller.C) initial_storage_caller 0tez in
  let contr = Test.to_contract caller_addr in
  let () = Test.set_source owner1 in
  let _ = Test.transfer_exn token_addr
    (Update_operators ([
      (Add_operator ({
        owner    = owner1;
        operator = (Test.to_address caller_addr);
        token_id = 0n;
      } : Token.FA2.SingleAssetExtendable.TZIP12.operator) : Token.FA2.SingleAssetExtendable.TZIP12.unit_update);
    ] : Token.FA2.SingleAssetExtendable.TZIP12.update_operators)) 0tez in
  let r = Test.transfer_to_contract contr (Deposit 5n ) 0tez in
  Assert.string_failure r Caller.C.Errors.too_late

  let test_deposit_fail_operator =
  let initial_date : timestamp = ("2024-02-01t00:00:00Z" : timestamp) in
  let start_date : timestamp = ("2024-01-01t00:00:00Z" : timestamp) in
  let end_date : timestamp = ("2024-03-01t00:00:00Z" : timestamp) in
  let freezing_duration : int = 86_400 * 15 in
  let initial_storage, owners, operators = get_initial_storage (initial_date, 10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let _op1    = List_helper.nth_exn 0 operators in
  let { addr = token_addr;code = _code; size = _size} = Test.originate (contract_of Token) initial_storage 0tez in
  let initial_storage_caller = {ledger = Big_map.literal[]; admin = owner1; token_address = Test.to_address token_addr;start_date=start_date;end_date=end_date;freezing_duration=freezing_duration} in
  let { addr = caller_addr;code = _code; size = _size} = Test.originate (contract_of Caller.C) initial_storage_caller 0tez in
  let contr = Test.to_contract caller_addr in
  let () = Test.set_source owner1 in
  let r = Test.transfer_to_contract contr (Deposit 5n ) 0tez in
  Assert.string_failure r Token.FA2.SingleAssetExtendable.Errors.not_operator

  let test_deposit_fail_balance =
  let initial_date : timestamp = ("2024-02-01t00:00:00Z" : timestamp) in
  let start_date : timestamp = ("2024-01-01t00:00:00Z" : timestamp) in
  let end_date : timestamp = ("2024-03-01t00:00:00Z" : timestamp) in
  let freezing_duration : int = 86_400 * 15 in
  let initial_storage, owners, operators = get_initial_storage (initial_date, 0n, 0n, 0n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let _op1    = List_helper.nth_exn 0 operators in
  let { addr = token_addr;code = _code; size = _size} = Test.originate (contract_of Token) initial_storage 0tez in
  let initial_storage_caller = {ledger = Big_map.literal[]; admin = owner1; token_address = Test.to_address token_addr;start_date=start_date;end_date=end_date;freezing_duration=freezing_duration} in
  let { addr = caller_addr;code = _code; size = _size} = Test.originate (contract_of Caller.C) initial_storage_caller 0tez in
  let contr = Test.to_contract caller_addr in
  let () = Test.set_source owner1 in
  let _ = Test.transfer_exn token_addr
    (Update_operators ([
      (Add_operator ({
        owner    = owner1;
        operator = (Test.to_address caller_addr);
        token_id = 0n;
      } : Token.FA2.SingleAssetExtendable.TZIP12.operator) : Token.FA2.SingleAssetExtendable.TZIP12.unit_update);
    ] : Token.FA2.SingleAssetExtendable.TZIP12.update_operators)) 0tez in
  let r = Test.transfer_to_contract contr (Deposit 15n ) 0tez  in  
  Assert.string_failure r Token.FA2.SingleAssetExtendable.Errors.ins_balance

  let test_claim_success =
  let initial_date : timestamp = ("2024-02-01t00:00:00Z" : timestamp) in
  let start_date : timestamp = ("2024-01-01t00:00:00Z" : timestamp) in
  let end_date : timestamp = ("2024-03-01t00:00:00Z" : timestamp) in
  let freezing_duration : int = 86_400 * 15 in
  let initial_storage, owners, operators = get_initial_storage (initial_date, 10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let _op1    = List_helper.nth_exn 0 operators in
  let { addr = token_addr;code = _code; size = _size} = Test.originate (contract_of Token) initial_storage 0tez in
  let initial_storage_caller = {ledger = Big_map.literal[]; admin = owner1; token_address = Test.to_address token_addr;start_date=start_date;end_date=end_date;freezing_duration=freezing_duration} in
  let { addr = caller_addr;code = _code; size = _size} = Test.originate (contract_of Caller.C) initial_storage_caller 0tez in
  let contr = Test.to_contract caller_addr in
  let () = Test.set_source owner1 in
  let _ = Test.transfer_exn token_addr
    (Update_operators ([
      (Add_operator ({
        owner    = owner1;
        operator = (Test.to_address caller_addr);
        token_id = 0n;
      } : Token.FA2.SingleAssetExtendable.TZIP12.operator) : Token.FA2.SingleAssetExtendable.TZIP12.unit_update);
    ] : Token.FA2.SingleAssetExtendable.TZIP12.update_operators)) 0tez in
  let _ = Test.transfer_to_contract_exn contr (Deposit 5n ) 0tez in
  let _ = Test.transfer_to_contract_exn contr (Claim 5n ) 0tez in
  let () = assert_caller_balances caller_addr (owner1, 0n) in
  let () = assert_balances token_addr ((owner1, 10n), (owner2, 10n), (owner3, 10n)) in
  ()

  let test_claim_fail_balance =
  let initial_date : timestamp = ("2024-02-01t00:00:00Z" : timestamp) in
  let start_date : timestamp = ("2024-01-01t00:00:00Z" : timestamp) in
  let end_date : timestamp = ("2024-03-01t00:00:00Z" : timestamp) in
  let freezing_duration : int = 86_400 * 15 in
  let initial_storage, owners, operators = get_initial_storage (initial_date, 10n, 10n, 10n) in
  let owner1 = List_helper.nth_exn 0 owners in
  let _owner2 = List_helper.nth_exn 1 owners in
  let _owner3 = List_helper.nth_exn 2 owners in
  let _op1    = List_helper.nth_exn 0 operators in
  let { addr = token_addr;code = _code; size = _size} = Test.originate (contract_of Token) initial_storage 0tez in
  let initial_storage_caller = {ledger = Big_map.literal[]; admin = owner1; token_address = Test.to_address token_addr;start_date=start_date;end_date=end_date;freezing_duration=freezing_duration} in
  let { addr = caller_addr;code = _code; size = _size} = Test.originate (contract_of Caller.C) initial_storage_caller 0tez in
  let contr = Test.to_contract caller_addr in
  let () = Test.set_source owner1 in
  let _ = Test.transfer_exn token_addr
    (Update_operators ([
      (Add_operator ({
        owner    = owner1;
        operator = (Test.to_address caller_addr);
        token_id = 0n;
      } : Token.FA2.SingleAssetExtendable.TZIP12.operator) : Token.FA2.SingleAssetExtendable.TZIP12.unit_update);
    ] : Token.FA2.SingleAssetExtendable.TZIP12.update_operators)) 0tez in
  let _ = Test.transfer_to_contract_exn contr (Deposit 5n ) 0tez in
  let r = Test.transfer_to_contract contr (Claim 10n ) 0tez in
  Assert.string_failure r Caller.C.Errors.ins_balance