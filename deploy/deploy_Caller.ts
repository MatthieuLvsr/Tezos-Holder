import { InMemorySigner } from "@taquito/signer";
import { MichelsonMap, TezosToolkit } from "@taquito/taquito";
import { char2Bytes } from "@taquito/utils";
import * as dotenv from "dotenv";

dotenv.config();

const SECRET_KEY = process.env.SECRET_KEY;
const ADMIN_ADDRESS = process.env.ADMIN_ADDRESS;

import Caller from "../compiled/Caller.mligo.json";

const RPC_ENDPOINT = "https://ghostnet.tezos.marigold.dev";

export async function DeployCaller(
  token_address: string
): Promise<string | void> {
  if (!SECRET_KEY || !ADMIN_ADDRESS) return;
  const Tezos = new TezosToolkit(RPC_ENDPOINT);

  //set alice key
  Tezos.setProvider({
    signer: await InMemorySigner.fromSecretKey(SECRET_KEY),
  });

  const initialStorage = {
    admin: ADMIN_ADDRESS,
    end_date: new Date("2024-03-01T00:00:00Z"),
    freezing_duration: 1296000,
    ledger: new Map([]),
    start_date: new Date("2024-01-01T00:00:00Z"),
    token_address,
  };

  try {
    const originated = await Tezos.contract.originate({
      code: Caller,
      storage: initialStorage,
    });
    console.log(
      `Waiting for myContract ${originated.contractAddress} to be confirmed...`
    );
    await originated.confirmation(2);
    console.log("confirmed contract: ", originated.contractAddress);
    return originated.contractAddress;
  } catch (error: any) {
    console.log(error);
  }
}
