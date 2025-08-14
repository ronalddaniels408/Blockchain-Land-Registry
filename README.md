# 🏡 Blockchain Land Registry

A decentralized land registry system built on Stacks blockchain using Clarity smart contracts.

## 🎯 Features

- NFT-based land parcel registration
- Secure ownership transfers
- GPS coordinates and legal document tracking
- DAO-governed dispute resolution
- Complete transaction history

## 🛠 Technical Implementation

The smart contract implements:
- Land parcel registration with GPS coordinates and legal documentation
- Secure ownership transfer mechanism
- DAO member management
- Detailed property information tracking

## 📝 Usage

### Register a Land Parcel
```clarity
(contract-call? .land-registry register-land-parcel u1 -74659802 40771912 u1000 "QmHash...")
```

### Initiate Transfer
```clarity
(contract-call? .land-registry initiate-transfer u1 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM u100000000)
```

### Accept Transfer
```clarity
(contract-call? .land-registry accept-transfer u1)
```

### Query Land Details
```clarity
(contract-call? .land-registry get-land-details u1)
```

## 🔐 Security

- Only authorized users can register land parcels
- Transfer requires both sender and receiver confirmation
- DAO governance for dispute resolution
- Immutable transaction history

## 🤝 Contributing

Pull requests are welcome! Please ensure your changes maintain the security and integrity of the system.
```

