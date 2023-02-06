// SPDX-License-Identifier: MIT
pragma solidity 0.8.17;


/**
 @author Tellor Inc.
 @title Autopay
 @dev This is a contract for automatically paying for Tellor oracle data at
 * specific time intervals, as well as one time tips.
*/
contract Autopay is UsingTellor {
    // Storage
    IERC20 public token; // TRB token address
    IQueryDataStorage public queryDataStorage; // Query data storage contract
    uint256 public fee; // 1000 is 100%, 50 is 5%, etc.

    mapping(bytes32 => uint256) public queryIdsWithFundingIndex; // mapping queryId to queryIdsWithFunding index plus one (0 if not in array)
    mapping(bytes32 => Tip[]) public tips; // mapping queryId to tips
    mapping(address => uint256) public userTipsTotal; // track user tip total per user

    bytes32[] public queryIdsWithFunding; // array of queryIds that have funding

    // Structs
    struct SingleTipsWithQueryData {
        bytes queryData; // query data with single tip for requested data
        uint256 tip; // reward amount for request
    }

    struct Tip {
        uint256 amount; // amount tipped
        uint256 timestamp; // time tipped
        uint256 cumulativeTips; // cumulative tips for query ID
    }

    // Events
    event OneTimeTipClaimed(
        bytes32 indexed _queryId,
        uint256 indexed _amount,
        address indexed _reporter
    );
    event TipAdded(
        bytes32 indexed _queryId,
        uint256 indexed _amount,
        bytes _queryData,
        address _tipper
    );

    // Functions
    /**
     * @dev Initializes system parameters
     * @param _tellor address of Tellor contract
     * @param _queryDataStorage address of query data storage contract
     * @param _fee percentage, 1000 is 100%, 50 is 5%, etc.
     */
    constructor(
        address payable _tellor,
        address _queryDataStorage,
        uint256 _fee
    ) UsingTellor(_tellor) {
        token = IERC20(tellor.token());
        queryDataStorage = IQueryDataStorage(_queryDataStorage);
        fee = _fee;
    }

    /**
     * @dev Function to claim singular tip
     * @param _queryId id of reported data
     * @param _timestamps[] batch of timestamps array of reported data eligible for reward
     */
    function claimOneTimeTip(bytes32 _queryId, uint256[] calldata _timestamps)
        external
    {
        require(
            tips[_queryId].length > 0,
            "no tips submitted for this queryId"
        );
        uint256 _cumulativeReward;
        for (uint256 _i = 0; _i < _timestamps.length; _i++) {
            _cumulativeReward += _getOneTimeTipAmount(
                _queryId,
                _timestamps[_i]
            );
        }
        require(
            token.transfer(
                msg.sender,
                _cumulativeReward - ((_cumulativeReward * fee) / 1000)
            )
        );
        token.approve(address(tellor), (_cumulativeReward * fee) / 1000);
        tellor.addStakingRewards((_cumulativeReward * fee) / 1000);
        if (getCurrentTip(_queryId) == 0) {
            if (queryIdsWithFundingIndex[_queryId] != 0) {
                uint256 _idx = queryIdsWithFundingIndex[_queryId] - 1;
                // Replace unfunded feed in array with last element
                queryIdsWithFunding[_idx] = queryIdsWithFunding[
                    queryIdsWithFunding.length - 1
                ];
                bytes32 _queryIdLastFunded = queryIdsWithFunding[_idx];
                queryIdsWithFundingIndex[_queryIdLastFunded] = _idx + 1;
                queryIdsWithFundingIndex[_queryId] = 0;
                queryIdsWithFunding.pop();
            }
        }
        emit OneTimeTipClaimed(_queryId, _cumulativeReward, msg.sender);
    }

    /**
     * @dev Function to run a single tip
     * @param _queryId ID of tipped data
     * @param _amount amount to tip
     * @param _queryData the data used by reporters to fulfill the query
     */
    function tip(
        bytes32 _queryId,
        uint256 _amount,
        bytes calldata _queryData
    ) external {
        require(
            _queryId == keccak256(_queryData),
            "id must be hash of bytes data"
        );
        require(_amount > 0, "tip must be greater than zero");
        Tip[] storage _tips = tips[_queryId];
        if (_tips.length == 0) {
            _tips.push(Tip(_amount, block.timestamp, _amount));
            queryDataStorage.storeData(_queryData);
        } else {
            (, uint256 _timestampRetrieved) = _getCurrentValue(_queryId);
            if (_timestampRetrieved < _tips[_tips.length - 1].timestamp) {
                _tips[_tips.length - 1].timestamp = block.timestamp;
                _tips[_tips.length - 1].amount += _amount;
                _tips[_tips.length - 1].cumulativeTips += _amount;
            } else {
                _tips.push(
                    Tip(
                        _amount,
                        block.timestamp,
                        _tips[_tips.length - 1].cumulativeTips + _amount
                    )
                );
            }
        }
        if (
            queryIdsWithFundingIndex[_queryId] == 0 &&
            getCurrentTip(_queryId) > 0
        ) {
            queryIdsWithFunding.push(_queryId);
            queryIdsWithFundingIndex[_queryId] = queryIdsWithFunding.length;
        }
        require(
            token.transferFrom(msg.sender, address(this), _amount),
            "ERC20: transfer amount exceeds balance"
        );
        userTipsTotal[msg.sender] += _amount;
        emit TipAdded(_queryId, _amount, _queryData, msg.sender);
    }

    // Getters
    /**
     * @dev Getter function to current oneTime tip by queryId
     * @param _queryId id of reported data
     * @return amount of tip
     */
    function getCurrentTip(bytes32 _queryId) public view returns (uint256) {
        // if no tips, return 0
        if (tips[_queryId].length == 0) {
            return 0;
        }
        (, uint256 _timestampRetrieved) = _getCurrentValue(_queryId);
        Tip memory _lastTip = tips[_queryId][tips[_queryId].length - 1];
        if (_timestampRetrieved < _lastTip.timestamp) {
            return _lastTip.amount;
        } else {
            return 0;
        }
    }
    /**
     * @dev Getter function for queryIds with current one time tips
     */
    function getFundedQueryIds() external view returns (bytes32[] memory) {
        return queryIdsWithFunding;
    }
    /**
     * @dev Getter function to get number of past tips
     * @param _queryId id of reported data
     * @return count of tips available
     */
    function getPastTipCount(bytes32 _queryId) external view returns (uint256) {
        return tips[_queryId].length;
    }

    /**
     * @dev Getter function for past tips
     * @param _queryId id of reported data
     * @return Tip struct (amount/timestamp) of all past tips
     */
    function getPastTips(bytes32 _queryId)
        external
        view
        returns (Tip[] memory)
    {
        return tips[_queryId];
    }

    /**
     * @dev Getter function for past tips by index
     * @param _queryId id of reported data
     * @param _index uint index in the Tip array
     * @return amount/timestamp of specific tip
     */
    function getPastTipByIndex(bytes32 _queryId, uint256 _index)
        external
        view
        returns (Tip memory)
    {
        return tips[_queryId][_index];
    }
    /**
     * @dev Getter function for retrieving the total amount of tips paid by a given address
     * @param _user address of user to query
     * @return uint256 total amount of tips paid by user
     */
    function getTipsByAddress(address _user) external view returns (uint256) {
        return userTipsTotal[_user];
    }

    // Internal functions
    /**
     ** @dev Internal function which determines tip eligibility for a given oracle submission
     * @param _queryId id of reported data
     * @param _timestamp timestamp of one time tip
     * @return _tipAmount of tip
     */
    function _getOneTimeTipAmount(bytes32 _queryId, uint256 _timestamp)
        internal
        returns (uint256 _tipAmount)
    {
        require(
            block.timestamp - _timestamp > 12 hours,
            "buffer time has not passed"
        );
        require(!isInDispute(_queryId, _timestamp), "value disputed");
        require(
            msg.sender == getReporterByTimestamp(_queryId, _timestamp),
            "msg sender must be reporter address"
        );
        Tip[] storage _tips = tips[_queryId];
        uint256 _min = 0;
        uint256 _max = _tips.length;
        uint256 _mid;
        while (_max - _min > 1) {
            _mid = (_max + _min) / 2;
            if (_tips[_mid].timestamp > _timestamp) {
                _max = _mid;
            } else {
                _min = _mid;
            }
        }
        (, uint256 _timestampBefore) = getDataBefore(_queryId, _timestamp);
        require(
            _timestampBefore < _tips[_min].timestamp,
            "tip earned by previous submission"
        );
        require(
            _timestamp >= _tips[_min].timestamp,
            "timestamp not eligible for tip"
        );
        require(_tips[_min].amount > 0, "tip already claimed");
        _tipAmount = _tips[_min].amount;
        _tips[_min].amount = 0;
        uint256 _minBackup = _min;
        // check whether eligible for previous tips in array due to disputes
        (, uint256 _indexNow) = getIndexForDataBefore(_queryId, _timestamp + 1);
        (bool _found, uint256 _indexBefore) = getIndexForDataBefore(
            _queryId,
            _timestampBefore + 1
        );
        if (_indexNow - _indexBefore > 1 || !_found) {
            if (!_found) {
                _tipAmount = _tips[_minBackup].cumulativeTips;
            } else {
                _max = _min;
                _min = 0;
                _mid;
                while (_max - _min > 1) {
                    _mid = (_max + _min) / 2;
                    if (_tips[_mid].timestamp > _timestampBefore) {
                        _max = _mid;
                    } else {
                        _min = _mid;
                    }
                }
                _min++;
                if (_min < _minBackup) {
                    _tipAmount =
                        _tips[_minBackup].cumulativeTips -
                        _tips[_min].cumulativeTips +
                        _tips[_min].amount;
                }
            }
        }
    }

    /**
     * @dev Allows the user to get the latest value for the queryId specified
     * @param _queryId is the id to look up the value for
     * @return _value the value retrieved
     * @return _timestampRetrieved the retrieved value's timestamp
     */

    function _getCurrentValue(bytes32 _queryId)
        internal
        view
        returns (bytes memory _value, uint256 _timestampRetrieved)
    {
        uint256 _count = getNewValueCountbyQueryId(_queryId);
        if (_count == 0) {
            return (bytes(""), 0);
        }
        uint256 _time;
        //loop handles for dispute (value = "" if disputed)
        while (_count > 0) {
            _count--;
            _time = getTimestampbyQueryIdandIndex(_queryId, _count);
            _value = retrieveData(_queryId, _time);
            if (_value.length > 0) {
                return (_value, _time);
            }
        }
        return (bytes(""), _time);
    }
}