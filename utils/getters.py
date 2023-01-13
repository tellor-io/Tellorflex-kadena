from utils.assemble_pactcode import assemble_code

# Getters (LOCAL!)
get_stake_amount = assemble_code(function="free.tellorflex.stake-amount")
get_nonce = assemble_code(function="free.tellorflex.get-new-value-count-by-query-id", queryId="")
get_staker_info = assemble_code(function="free.tellorflex.get-staker-info", staker="")
get_time_of_last_new_value = assemble_code(function="free.tellorflex.time-of-last-new-value")
get_total_time_based_rewards_balance = assemble_code(function="free.tellorflex.get-total-time-based-rewards-balance")

if __name__ == "__main__":
    print(get_stake_amount)