% create the common parameters used in all optimizations so can change everything in one place
function [options, cuts, vals, num_ps, A_moments, b_moments, A_ineq, b_ineq, x0_start, lb, ub] = get_mort_solver_params_nomon(input_csv, f2_limit);

    % return value: matrix with p value, lower bound, upper bound
    global A_moments b_moments f_min_mse
    
    % possibly unnecessary definition of global MSE minimum
    f_min_mse = 0;
    
    % set parameters calibrated for most efficient solution (original params from CR on following line)
    %options = optimoptions(@fmincon,'MaxFunEvals',10000000,'Display','none','TolCon',1e-6,'TolFun',1e-11,'TolX',1e-8);

    % parameter settings that work better with get_mort_bounds_seeds.m (decrease step size tolerance for smoother curves)
    options = optimoptions(@fmincon,'MaxFunEvals',10000000,'Display','none','TolCon',1e-6,'TolFun',1e-11,'TolX',1e-16);

    % read bin means and values and convert to a set of bin cuts and values
    [cuts, vals] = read_bins(input_csv);
    
    % define number of p-values (i.e. p1 -> p100)
    num_ps = 100;
    
    % generate matrix -- used internally by inequality constraint function
    [A_moments, b_moments] = get_moment_constraints(cuts, vals);
    
    num_bounds = size(cuts, 2);
    
    % create the numerical 2nd derivative matrix, which looks like this with 10 p-levels
    % A_f2    = [ 1 -2  1  0  0  0  0  0  0  0;
    %             0  1 -2  1  0  0  0  0  0  0;
    %             0  0  1 -2  1  0  0  0  0  0;
    %             0  0  0  1 -2  1  0  0  0  0;
    %             0  0  0  0  1 -2  1  0  0  0;
    %             0  0  0  0  0  1 -2  1  0  0;
    %             0  0  0  0  0  0  1 -2  1  0;
    %             0  0  0  0  0  0  0  1 -2  1;];
    e = eye(num_ps);
    A_f2 = e(1:(num_ps-2), :) + [zeros((num_ps - 2), 2) eye(num_ps - 2)] + [zeros((num_ps - 2), 1) -2*eye(num_ps - 2)  zeros((num_ps - 2),1)];
    
    % put positive and negative A_f2 in same matrix so we can use them with a single inequality constraint  
    A_f2_sym = [A_f2; -A_f2];
    
    % set b_f2 limit -- multiply by 2 since needs to line up with both positive and negative A_f2 matrix
    b_f2_sym = f2_limit * ones(2 * (num_ps - 2), 1);
    
    % This is non-monotonic function -- no slope constraints
    A_pos_slope = e(1:(num_ps - 1), :) + [zeros((num_ps - 1), 1) -eye(num_ps - 1)];
    b_pos_slope = zeros(num_ps - 1, 1);
    
    % define starting point for optimizer -- equal mortality in all bins
    x0_start = mean(vals) * ones(1, num_ps);
    
    % create inequality constraint vector: NOTE NO SLOPE CONSTRAINT HERE, f2 below threshold, MSE below threshold
    A_ineq = A_f2_sym;
    b_ineq = b_f2_sym;
    
    % set upper and lower bound vectors
    lb = zeros(1, num_ps) ;
    ub = 100000 * ones(1,num_ps); 
