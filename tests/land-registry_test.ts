import { Clarinet, Tx, Chain, Account, types } from 'https://deno.land/x/clarinet@v1.0.0/index.ts';
import { assertEquals } from 'https://deno.land/std@0.90.0/testing/asserts.ts';

Clarinet.test({
    name: "Ensure that property registration works",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        let deployer = accounts.get('deployer')!;
        let wallet1 = accounts.get('wallet_1')!;
        
        let block = chain.mineBlock([
            Tx.contractCall('land-registry', 'register-property', [
                types.ascii("123 Main St, City"),
                types.uint(1000),
                types.ascii("residential"),
                types.uint(500000)
            ], wallet1.address)
        ]);
        
        block.receipts[0].result.expectOk().expectUint(1);
    },
});

Clarinet.test({
    name: "Ensure that property transfer works",
    async fn(chain: Chain, accounts: Map<string, Account>) {
        let deployer = accounts.get('deployer')!;
        let wallet1 = accounts.get('wallet_1')!;
        let wallet2 = accounts.get('wallet_2')!;
        
        // First register a property
        let block1 = chain.mineBlock([
            Tx.contractCall('land-registry', 'register-property', [
                types.ascii("456 Oak Ave, Town"),
                types.uint(2000),
                types.ascii("commercial"),
                types.uint(1000000)
            ], wallet1.address)
        ]);
        
        block1.receipts[0].result.expectOk().expectUint(1);
        
        // Initiate transfer
        let block2 = chain.mineBlock([
            Tx.contractCall('land-registry', 'initiate-transfer', [
                types.uint(1),
                types.principal(wallet2.address),
                types.uint(1200000)
            ], wallet1.address)
        ]);
        
        block2.receipts[0].result.expectOk().expectBool(true);
        
        // Complete transfer
        let block3 = chain.mineBlock([
            Tx.contractCall('land-registry', 'complete-transfer', [
                types.uint(1)
            ], wallet2.address)
        ]);
        
        block3.receipts[0].result.expectOk().expectBool(true);
    },
});
