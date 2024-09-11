curl -X POST http://localhost:8080/reaction \
-H "Content-Type: application/json" \
-d '{
  "reaction": {
    "reactionId": 21,
    "reactionName": "2,3-Dimethylpentan-3-ol Synthesis"
  },
  "inboundReagents": [
    [
      {
        "reagentAmount": 1.0
      },
      {
        "moleculeId": 31,
        "moleculeSmiles": "CC(C)=C",
        "moleculeIupacName": "2,3-Dimethylbutene-2"
      }
    ],
    [
      {
        "reagentAmount": 1.0
      },
      {
        "moleculeId": 32,
        "moleculeSmiles": "CC(C)OCC",
        "moleculeIupacName": "Ethoxypropane"
      }
    ]
  ],
  "outboundProducts": [
    [
      {
        "productAmount": 1.0
      },
      {
        "moleculeId": 33,
        "moleculeSmiles": "CC(C)C(CO)CC",
        "moleculeIupacName": "2,3-Dimethylpentan-3-ol"
      }
    ]
  ],
  "conditions": [
    [
      {
        "temperature": [323.15],
        "pressure": [151.825]
      },
      {
        "catalystId": 32,
        "catalystSmiles": "Cl[Zn]",
        "catalystName": "Zinc Chloride"
      }
    ]
  ]
}'
