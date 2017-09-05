"use strict";

function determineLevelForEntry(entry) {
	var level = 'success';
	for (var i = 0; i < entry.issues.length; i++) {
		var issue = entry.issues[i];
		if (issue.severity == 'Warning') {
			level = 'warning';
		} else if (issue.severity == 'Error') {
			level = 'danger';
			break;
		}
	}
	return level;
}

function determineLevelForIssue(issue) {
	if (issue.severity == 'Error') {
		return 'danger';
	} else if (issue.severity == 'Warning') {
		return 'warning';
	} else {
		return 'info';
	}
}

function getHelpMessage(error_code) {
	if (error_code in report.help_messages) {
		return report.help_messages[error_code];
	} else {
		return ""
	}
}

function setProgress() {
	var noWarnings = true;
	var success = 0;
	var count = Object.keys(report.conversions).length;
	for (var addr in report.conversions) {
		var entry = report.conversions[addr];
		var level = determineLevelForEntry(entry);
		if (level != 'danger') {
			success++;
		}
		if (level == 'warning') {
			noWarnings = false;
		}
	}

	$('#progress-num').text(`${success} / ${count}`);

	var bar = $('#progress .progress-bar');

	if (success == count && count > 0 && noWarnings) {
		bar.addClass('progress-bar-success');
	} else if (success == count && count > 0) {
		bar.addClass('progress-bar-warning');
	} else if (count > 0) {
		bar.addClass('progress-bar-danger');
	}

	var percent = 100.0;
	if (count > 0) {
		percent = 100.0 * (success / count);
	}
	bar.attr('style', `width: ${percent}%`)
}

function create_source(addr, source) {
	return $('<pre>').append(
		$('<div>').addClass('nodeaddress').text(`${addr}`),
		source
	);
}

function createTrace(addr, issue_index, issue, trace, index) {
	var id = `entry-${addr}-issue-${issue_index}-backtrace-${index}`;

	var title = trace.address;

	if (trace.location) {
		title = trace.location;
	}

	var $body = $('<div>').addClass('panel-body');

	if (trace.source) {
		$body.append(create_source(addr, trace.source));
	}

	if (!$body.html()) {
		$body.html("<i>No location information.</i>");
	}

	return $('<div>')
		.addClass('panel panel-default')
		.addClass(!trace.location ? 'internal' : '')
		.append(
			$('<div>')
				.addClass('panel-heading')
				.append(
					$('<a>')
						.attr('href', `#${id}`)
						.attr('data-toggle', 'collapse')
						.attr('data-parent', `#entry-${addr}-issue-${issue_index}-backtrace`)
						.text(title)
				),
			$('<div>')
				.attr('id', id)
				.addClass('panel-collapse collapse')
				.append($body)
		);
}

function createIssue(addr, issue, index) {
	return $('<div>')
		.addClass(`entry-issue panel panel-${determineLevelForIssue(issue)}`)
		.append(
			$('<div>')
				.addClass('panel-heading')
				.append(
					$('<div>')
						.addClass('entry-issue-error-code')
						.text(issue.error_code),
					$('<div>')
						.addClass('entry-issue-message')
						.append(
							$('<a>')
								.attr('href', `#entry-${addr}-issue-${index}`)
								.attr('data-toggle', 'collapse')
								.addClass('panel-title')
								.html(`<strong>${issue.severity}:</strong> ${issue.message}`)
						),
					$('<div>')
						.addClass('entry-issue-category')
						.append(
							$('<div>')
							.addClass('label label-default')
							.text(issue.category)
						)
				),
			$('<div>')
				.attr('id', `entry-${addr}-issue-${index}`)
				.addClass('entry-issue-body collapse')
				.append(
					$('<div>')
						.addClass('panel-body')
						.append(
							$('<div>')
								.addClass('entry-issue-help')
								.text(getHelpMessage(issue.error_code)),
							create_source(addr, issue.loc.source),
							$('<div>')
								.attr('id', `entry-${addr}-issue-${index}-backtrace`)
								.addClass('panel-group')
								.addClass('backtrace')
								.append(
									$('<div>').addClass('backtrace-text').text('Backtrace'),
									$.map(issue.backtrace, $.proxy(createTrace, null, addr, index, issue))
								)
						)
				)
		);
}

function createConversion(entry, addr) {
	return $('<div>')
		.addClass(`entry panel panel-${determineLevelForEntry(entry)}`)
		.append(
			$('<div>')
				.addClass('panel-heading')
				.append(
					$('<i>').addClass('glyphicon glyphicon-file'),
					' ',
					$('<a>')
						.attr('href', `#entry-${addr}`)
						.attr('data-toggle', 'collapse')
						.addClass('panel-title')
						.text(entry.loc.location)
				),
			$('<div>')
				.attr('id', `entry-${addr}`)
				.addClass('collapse')
				.append(
					$('<div>')
						.addClass('panel-body')
						.append(
							$.map(entry.issues, $.proxy(createIssue, null, addr)),
							create_source(addr, entry.loc.source)
						)
				)
		);
}

function createRaw() {
	return $('<div>')
		.addClass('panel panel-default')
		.append(
			$('<div>')
				.addClass('panel-heading')
				.attr('style', 'text-align: center')
				.append(
					$('<a>')
						.attr('href', '#raw')
						.attr('data-toggle', 'collapse')
						.text('Raw Conversion Report')
				),
			$('<div>')
				.attr('id', 'raw')
				.addClass('collapse')
				.append(
					$('<div>')
						.addClass('panel-body')
						.append(
							$('<pre>').text(JSON.stringify(report, null, 2))
						)
				)
		);
}

function setupControls() {
	var $internals = $('.internal').add('.nodeaddress');

	$internals.hide();

	$('#internal-button').click(function() {
		$internals.toggle();
	});
}

function main() {
	$('#main').append($.map(report.conversions, createConversion));

	// open if only 1
	if (Object.keys(report.conversions).length == 1) {
		for (var addr in report.conversions) {
			$(`#entry-${addr}`).collapse('show');
		}
	}

	// add raw block
	$('#main').append(createRaw());

	setProgress();
	setupControls();
}

main();
