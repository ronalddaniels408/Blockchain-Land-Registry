import { describe, it, expect, beforeEach } from 'vitest';
import { Cl } from '@stacks/transactions';
import { initSimnet } from '@hirosystems/clarinet-sdk';

const simnet = await initSimnet();

const accounts = simnet.getAccounts();
const deployer = accounts.get('deployer')!;
const wallet1 = accounts.get('wallet_1')!;
const wallet2 = accounts.get('wallet_2')!;

describe('Land Registry Contract', () => {
  beforeEach(() => {
    simnet.clearDeploymentData();
  });

  it('should register a property successfully', () => {
    const { result } = simnet.callPublicFn('land-registry', 'register-property', [
      Cl.stringAscii('123 Main St, City'),
      Cl.uint(1000),
      Cl.stringAscii('residential'),
      Cl.uint(500000)
    ], wallet1);
    
    expect(result).toBeOk(Cl.uint(1));
  });

  it('should handle property transfer workflow', () => {
    // First register a property
    const { result: registerResult } = simnet.callPublicFn('land-registry', 'register-property', [
      Cl.stringAscii('456 Oak Ave, Town'),
      Cl.uint(2000),
      Cl.stringAscii('commercial'),
      Cl.uint(1000000)
    ], wallet1);
    
    expect(registerResult).toBeOk(Cl.uint(1));
    
    // Initiate transfer
    const { result: initiateResult } = simnet.callPublicFn('land-registry', 'initiate-transfer', [
      Cl.uint(1),
      Cl.principal(wallet2),
      Cl.uint(1200000)
    ], wallet1);
    
    expect(initiateResult).toBeOk(Cl.bool(true));
    
    // Complete transfer
    const { result: completeResult } = simnet.callPublicFn('land-registry', 'complete-transfer', [
      Cl.uint(1)
    ], wallet2);
    
    expect(completeResult).toBeOk(Cl.bool(true));
  });
});
