# Auditrax

# 🔍 Auditrax - DAO Audit Protocol

> **Hire auditors through trustless contracts** 🤝

Auditrax is a decentralized platform that connects DAOs with professional auditors through smart contracts, ensuring transparent, secure, and trustless audit processes.

## 🌟 Features

- **🏢 DAO Registration**: DAOs can register and create audit requests
- **👨‍💻 Auditor Profiles**: Auditors can register with reputation tracking
- **💰 Escrow System**: Secure payment handling with platform fees
- **📋 Application Process**: Auditors can apply for audit opportunities
- **✅ Approval Workflow**: Complete audit lifecycle management
- **🏆 Reputation System**: Track auditor performance and success rates

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

1. Clone the repository
2. Navigate to the project directory
3. Run Clarinet commands

```bash
clarinet check
```

```bash
clarinet test
```

```bash
clarinet console
```

## 📖 Usage

### For DAOs

1. **Register your DAO**:
   ```clarity
   (contract-call? .Auditrax register-dao "My DAO")
   ```

2. **Create an audit request**:
   ```clarity
   (contract-call? .Auditrax create-audit-request 
     "Smart Contract Security Audit" 
     "Full security review of our DeFi protocol" 
     u5000000 
     u1000)
   ```

3. **Assign an auditor**:
   ```clarity
   (contract-call? .Auditrax assign-auditor u1 'ST1AUDITOR...)
   ```
